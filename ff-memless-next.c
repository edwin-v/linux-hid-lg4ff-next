/************
 * WARNING! *
 ************
 * 
 * This is not the official ff-memless-next.c source!
 * I just have this here for my personal experiments. Consider it
 * broken.
 * 
 * Check the readme for more information about the real ff-memless-next.
 */



/*
 * Force feedback support for memoryless devices
 *
 * This module is based on "ff-memless" orignally written by Anssi Hannula.
 * It is extended to support all force feedback effects currently supported
 * by the Linux input stack.
 * Logic of emulation of FF_RUMBLE through FF_PERIODIC provided by
 * Elias Vanderstuyft <elias.vds@gmail.com>
 *
 * Copyright(c) 2014 Michal "MadCatX" Maly <madcatxster@devoid-pointer.net>
 *
 */

#define pr_fmt(fmt) KBUILD_MODNAME ": " fmt

#include <linux/slab.h>
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/spinlock.h>
#include <linux/jiffies.h>
#include <linux/fixp-arith.h>
#include <linux/input/ff-memless-next.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Michal \"MadCatX\" Maly");
MODULE_DESCRIPTION("Force feedback support for memoryless force feedback devices");

#define FF_MAX_EFFECTS 16
#define FF_MIN_EFFECT_LENGTH ((MSEC_PER_SEC / CONFIG_HZ) + 1)
#define FF_EFFECT_STARTED 1
#define FF_EFFECT_PLAYING 2
#define FF_EFFECT_EMULATED 3

enum mlnx_emulate {
	EMUL_NOTHING,	/* Do not emulate anything */
	EMUL_RUMBLE,	/* Emulate FF_RUMBLE with FF_PERIODIC */
	EMUL_PERIODIC	/* Emulate FF_PERIODIC with FF_RUMBLE */
};

struct mlnx_effect {
	struct ff_effect effect;
	struct ff_effect base_effect;
	unsigned long flags;
	unsigned long begin_at;
	unsigned long stop_at;
	unsigned long updated_at;
	unsigned long attack_stop;
	unsigned long fade_begin;
	int repeat;
};

struct mlnx_device {
	u8 combinable_playing;
	u8 rumble_playing;
	unsigned long update_rate_jiffies;
	void *private;
	struct mlnx_effect effects[FF_MAX_EFFECTS];
	u16 gain;
	struct timer_list timer;
	struct input_dev *dev;
	enum mlnx_emulate emul;

	int (*control_effect)(struct input_dev *, void *,
			      const struct mlnx_effect_command *);
};

static s32 mlnx_calculate_x_force(const s32 level,
						  const u16 direction)
{
	s32 new = (level * -fixp_sin(direction)) >> FRAC_N;
	pr_debug("x force: %d\n", new);
	return new;
}

static s32 mlnx_calculate_y_force(const s32 level,
						  const u16 direction)
{
	s32 new = (level * fixp_cos(direction)) >> FRAC_N;
	pr_debug("y force: %d\n", new);
	return new;
}

static bool mlnx_is_combinable(const struct ff_effect *effect)
{
	switch (effect->type) {
	case FF_CONSTANT:
	case FF_PERIODIC:
	case FF_RAMP:
		return true;
	default:
		return false;
	}
}

static bool mlnx_is_conditional(const struct ff_effect *effect)
{
	switch (effect->type) {
	case FF_DAMPER:
	case FF_FRICTION:
	case FF_INERTIA:
	case FF_SPRING:
		return true;
	default:
		return false;
	}
}

static void mlnx_clr_emulated(struct mlnx_effect *mlnxeff)
{
	__clear_bit(FF_EFFECT_EMULATED, &mlnxeff->flags);
}

static void mlnx_clr_playing(struct mlnx_effect *mlnxeff)
{
	__clear_bit(FF_EFFECT_PLAYING, &mlnxeff->flags);
}

static void mlnx_clr_started(struct mlnx_effect *mlnxeff)
{
	__clear_bit(FF_EFFECT_STARTED, &mlnxeff->flags);
}

static bool mlnx_is_emulated(const struct mlnx_effect *mlnxeff)
{
	return test_bit(FF_EFFECT_EMULATED, &mlnxeff->flags);
}

static bool mlnx_is_rumble(const struct ff_effect *effect)
{
	return effect->type == FF_RUMBLE;
}

static bool mlnx_is_playing(const struct mlnx_effect *mlnxeff)
{
	return test_bit(FF_EFFECT_PLAYING, &mlnxeff->flags);
}

static bool mlnx_is_started(const struct mlnx_effect *mlnxeff)
{
	return test_bit(FF_EFFECT_STARTED, &mlnxeff->flags);
}

static bool mlnx_test_set_playing(struct mlnx_effect *mlnxeff)
{
	return test_and_set_bit(FF_EFFECT_PLAYING, &mlnxeff->flags);
}

static const struct ff_envelope *mlnx_get_envelope(const struct ff_effect *effect)
{
	static const struct ff_envelope empty;

	switch (effect->type) {
	case FF_CONSTANT:
		return &effect->u.constant.envelope;
	case FF_PERIODIC:
		return &effect->u.periodic.envelope;
	case FF_RAMP:
		return &effect->u.ramp.envelope;
	default:
		return &empty;
	}
}

/* Some devices might have a limit on how many uncombinable effects
 * can be played at once */
static int mlnx_upload_conditional(struct mlnx_device *mlnxdev,
				   struct mlnx_effect *mlnxeff)
{
	struct mlnx_effect_command ecmd = {
		.cmd = MLNX_UPLOAD_UNCOMB,
		.u.uncomb.id = mlnxeff->effect.id,
		.u.uncomb.effect = &mlnxeff->effect
	};
	int i, gain = mlnxdev->gain;
	
	/* apply gain to effect */
	printk(KERN_DEBUG "Apply gain %i to condition effect %i\n", mlnxdev->gain, mlnxeff->effect.id);
	for (i=0; i<2; i++) {
		struct ff_condition_effect *cond_b = &mlnxeff->base_effect.u.condition[i];
		struct ff_condition_effect *cond_a = &mlnxeff->effect.u.condition[i];
		cond_a->right_coeff = cond_b->right_coeff * gain / 0xFFFF;
		cond_a->left_coeff = cond_b->left_coeff * gain / 0xFFFF;
		cond_a->right_saturation = cond_b->right_saturation * gain / 0xFFFF;
		cond_a->left_saturation = cond_b->left_saturation * gain / 0xFFFF;
	}
	return mlnxdev->control_effect(mlnxdev->dev, mlnxdev->private, &ecmd);
}

static int mlnx_erase_conditional(struct mlnx_device *mlnxdev,
				  const struct ff_effect *effect)
{
	struct mlnx_effect_command ecmd = {
		.cmd = MLNX_ERASE_UNCOMB,
		.u.uncomb.id = effect->id,
		.u.uncomb.effect = effect
	};
	return mlnxdev->control_effect(mlnxdev->dev, mlnxdev->private, &ecmd);
}

static void mlnx_set_envelope_times(struct mlnx_effect *mlnxeff)
{
	const struct ff_effect *effect = &mlnxeff->effect;
	const struct ff_envelope *envelope = mlnx_get_envelope(effect);

	if (envelope->attack_length) {
		unsigned long j = msecs_to_jiffies(envelope->attack_length);
		mlnxeff->attack_stop = mlnxeff->begin_at + j;
	}
	if (effect->replay.length && envelope->fade_length) {
		unsigned long j = msecs_to_jiffies(envelope->fade_length);
		mlnxeff->fade_begin = mlnxeff->stop_at - j;
	}
}

static void mlnx_set_trip_times(struct mlnx_effect *mlnxeff,
				const unsigned long now)
{
	const struct ff_effect *effect = &mlnxeff->effect;
	const u16 replay_delay = effect->replay.delay;
	const u16 replay_length = effect->replay.length;

	mlnxeff->begin_at = now + msecs_to_jiffies(replay_delay);
	mlnxeff->stop_at = mlnxeff->begin_at + msecs_to_jiffies(replay_length);
	mlnxeff->updated_at = mlnxeff->begin_at;
}

static void mlnx_start_effect(struct mlnx_effect *mlnxeff)
{
	const unsigned long now = jiffies;

	mlnx_set_trip_times(mlnxeff, now);
	mlnx_set_envelope_times(mlnxeff);
	__set_bit(FF_EFFECT_STARTED, &mlnxeff->flags);
}

static void mlnx_stop_effect(struct mlnx_device *mlnxdev,
			     const struct mlnx_effect *mlnxeff)
{
	switch (mlnxeff->effect.type) {
	case FF_PERIODIC:
		if (mlnx_is_emulated(mlnxeff)) {
			if (--mlnxdev->rumble_playing == 0) {
				const struct mlnx_effect_command c = {
					.cmd = MLNX_STOP_RUMBLE
				};
				mlnxdev->control_effect(mlnxdev->dev,
							mlnxdev->private, &c);
			}
			return;
		}
	case FF_CONSTANT:
	case FF_RAMP:
		if (--mlnxdev->combinable_playing == 0) {
			const struct mlnx_effect_command c = {
				.cmd = MLNX_STOP_COMBINED
			};
			mlnxdev->control_effect(mlnxdev->dev, mlnxdev->private,
						&c);
		}
		return;
	case FF_RUMBLE:
		if (mlnx_is_emulated(mlnxeff)) {
			if (--mlnxdev->combinable_playing == 0) {
				const struct mlnx_effect_command c = {
					.cmd = MLNX_STOP_COMBINED
				};
				mlnxdev->control_effect(mlnxdev->dev,
							mlnxdev->private, &c);
			}
		} else {
			if (--mlnxdev->rumble_playing == 0) {
				const struct mlnx_effect_command c = {
					.cmd = MLNX_STOP_RUMBLE
				};
				mlnxdev->control_effect(mlnxdev->dev,
							mlnxdev->private, &c);
			}
		}
		return;
	case FF_DAMPER:
	case FF_FRICTION:
	case FF_INERTIA:
	case FF_SPRING:
	{
		const struct mlnx_effect_command c = {
			.cmd = MLNX_STOP_UNCOMB,
			.u.uncomb.id = mlnxeff->effect.id,
			.u.uncomb.effect = &mlnxeff->effect
		};
		mlnxdev->control_effect(mlnxdev->dev, mlnxdev->private, &c);
		return;
	}
	default:
		return;
	}
}

static int mlnx_restart_effect(struct mlnx_device *mlnxdev,
			       struct mlnx_effect *mlnxeff)
{
	const struct ff_effect *effect = &mlnxeff->effect;
	const unsigned long now = jiffies;

	if (mlnx_is_combinable(effect)) {
		if (effect->replay.delay)
			mlnx_stop_effect(mlnxdev, mlnxeff);
		else {
			if (mlnx_is_emulated(mlnxeff))
				mlnxdev->rumble_playing--;
			else
				mlnxdev->combinable_playing--;
		}
	} else if (mlnx_is_rumble(effect)) {
		if (effect->replay.delay)
			mlnx_stop_effect(mlnxdev, mlnxeff);
		else {
			if (mlnx_is_emulated(mlnxeff))
				mlnxdev->combinable_playing--;
			else
				mlnxdev->rumble_playing--;
		}
	} else if (mlnx_is_conditional(effect)) {
		int ret;
		if (effect->replay.delay)
			mlnx_stop_effect(mlnxdev, mlnxeff);

		ret = mlnx_upload_conditional(mlnxdev, mlnxeff);
		if (ret)
			return ret;
	}

	mlnx_set_trip_times(mlnxeff, now);
	mlnx_set_envelope_times(mlnxeff);

	return 0;
}

static s32 mlnx_apply_envelope(const struct mlnx_effect *mlnxeff,
			       const s32 level)
{
	const struct ff_effect *effect = &mlnxeff->effect;
	const struct ff_envelope *envelope = mlnx_get_envelope(effect);
	const unsigned long now = jiffies;
	const s32 alevel = abs(level);

	/* Effect has an envelope with nonzero attack time */
	if (envelope->attack_length && time_before(now, mlnxeff->attack_stop)) {
		const s32 clength = jiffies_to_msecs(now - mlnxeff->begin_at);
		const s32 alength = envelope->attack_length;
		const s32 dlevel = (alevel - envelope->attack_level)
				 * clength / alength;
		return level < 0 ? -(dlevel + envelope->attack_level) :
				    (dlevel + envelope->attack_level);
	} else if (envelope->fade_length && time_before_eq(mlnxeff->fade_begin, now)) {
		const s32 clength = jiffies_to_msecs(now - mlnxeff->fade_begin);
		const s32 flength = envelope->fade_length;
		const s32 dlevel = (envelope->fade_level - alevel)
				 * clength / flength;
		return level < 0 ? -(dlevel + alevel) : (dlevel + alevel);
	}

	return level;
}

static s32 mlnx_calculate_periodic(struct mlnx_effect *mlnxeff, const s32 level)
{
	const struct ff_effect *effect = &mlnxeff->effect;
	const unsigned long now = jiffies;
	const u16 period = effect->u.periodic.period;
	const u16 phase = effect->u.periodic.phase;
	const s16 offset = effect->u.periodic.offset;
	s32 new = level;
	u16 t = (jiffies_to_msecs(now - mlnxeff->begin_at) + phase) % period;

	switch (effect->u.periodic.waveform) {
	case FF_SINE:
	{
		u16 degrees = (360 * t) / period;
		new = ((level * fixp_sin(degrees)) >> FRAC_N) + offset;
		break;
	}
	case FF_SQUARE:
	{
		u16 degrees = (360 * t) / period;
		new = level * (degrees < 180 ? 1 : -1) + offset;
		break;
	}
	case FF_SAW_UP:
		new = 2 * level * t / period - level + offset;
		break;
	case FF_SAW_DOWN:
		new = level - 2 * level * t / period + offset;
		break;
	case FF_TRIANGLE:
	{
		new = (2 * abs(level - (2 * level * t) / period));
		new = new - abs(level) + offset;
		break;
	}
	case FF_CUSTOM:
		pr_debug("Custom waveform is not handled by this driver\n");
		return level;
	default:
		pr_err("Invalid waveform\n");
		return level;
	}

	/* Ensure that the offset did not make the value exceed s16 range */
	new = clamp(new, -0x7fff, 0x7fff);
	pr_debug("level: %d, t: %u\n", new, t);
	return new;
}

static s32 mlnx_calculate_ramp(const struct mlnx_effect *mlnxeff)
{
	const struct ff_effect *effect = &mlnxeff->effect;
	const struct ff_envelope *envelope = mlnx_get_envelope(effect);
	const unsigned long now = jiffies;
	const u16 length = effect->replay.length;
	const s16 mean = (effect->u.ramp.start_level + effect->u.ramp.end_level) / 2;
	const u16 t = jiffies_to_msecs(now - mlnxeff->begin_at);
	s32 start = effect->u.ramp.start_level;
	s32 end = effect->u.ramp.end_level;
	s32 new;

	if (envelope->attack_length && time_before(now, mlnxeff->attack_stop)) {
		const s32 clength = jiffies_to_msecs(now - mlnxeff->begin_at);
		const s32 alength = envelope->attack_length;
		s32 attack_level;
		if (end > start)
			attack_level = mean - envelope->attack_level;
		else
			attack_level = mean + envelope->attack_level;
		start = (start - attack_level) * clength / alength
		      + attack_level;
	} else if (envelope->fade_length && time_before_eq(mlnxeff->fade_begin, now)) {
		const s32 clength = jiffies_to_msecs(now - mlnxeff->fade_begin);
		const s32 flength = envelope->fade_length;
		s32 fade_level;
		if (end > start)
			fade_level = mean + envelope->fade_level;
		else
			fade_level = mean - envelope->fade_level;
		end = (fade_level - end) * clength / flength + end;
	}

	new = ((end - start) * t) / length + start;
	new = clamp(new, -0x7fff, 0x7fff);
	pr_debug("RAMP level: %d, t: %u\n", new, t);
	return new;
}

static void mlnx_destroy(struct ff_device *dev)
{
	struct mlnx_device *mlnxdev = dev->private;
	del_timer_sync(&mlnxdev->timer);

	kfree(mlnxdev->private);
}

static unsigned long mlnx_get_envelope_update_time(const struct mlnx_effect *mlnxeff,
						   const unsigned long update_rate_jiffies)
{
	const struct ff_effect *effect = &mlnxeff->effect;
	const struct ff_envelope *envelope = mlnx_get_envelope(effect);
	const unsigned long now = jiffies;
	unsigned long fade_next;

	/* Effect has an envelope with nonzero attack time */
	if (envelope->attack_length && time_before(now, mlnxeff->attack_stop)) {
		if (time_before(mlnxeff->updated_at + update_rate_jiffies, mlnxeff->attack_stop))
			return mlnxeff->updated_at + update_rate_jiffies;

		return mlnxeff->attack_stop;
	}

	/* Effect has an envelope with nonzero fade time */
	if (mlnxeff->effect.replay.length) {
		if (!envelope->fade_length)
			return mlnxeff->stop_at;

		/* Schedule the next update when the fade begins */
		if (time_before(mlnxeff->updated_at, mlnxeff->fade_begin))
			return mlnxeff->fade_begin;

		/* Already fading */
		fade_next = mlnxeff->updated_at + update_rate_jiffies;

		/* Schedule update when the effect stops */
		if (time_after(fade_next, mlnxeff->stop_at))
			return mlnxeff->stop_at;
		/* Schedule update at the next checkpoint */
			return fade_next;
	}

	/* There is no envelope */

	/* Prevent the effect from being started twice */
	if (mlnxeff->begin_at == now && mlnx_is_playing(mlnxeff))
		return now - 1;

	return mlnxeff->begin_at;
}

static unsigned long mlnx_get_update_time(struct mlnx_effect *mlnxeff,
				const unsigned long update_rate_jiffies)
{
	const unsigned long now = jiffies;
	unsigned long time, update_periodic;

	switch (mlnxeff->effect.type) {
	/* Constant effect does not change with time, but it can have
	 * an envelope and a duration */
	case FF_CONSTANT:
		return mlnx_get_envelope_update_time(mlnxeff,
						     update_rate_jiffies);
	/* Periodic and ramp effects have to be periodically updated */
	case FF_PERIODIC:
	case FF_RAMP:
		time = mlnx_get_envelope_update_time(mlnxeff,
						     update_rate_jiffies);
		if (mlnx_is_emulated(mlnxeff))
			update_periodic = mlnxeff->stop_at;
		else
			update_periodic = mlnxeff->updated_at +
					  update_rate_jiffies;

		/* Periodic effect has to be updated earlier than envelope
		 * or envelope update time is in the past */
		if (time_before(update_periodic, time) || time_before(time, now))
			return update_periodic;
		/* Envelope needs to be updated */
		return time;
	case FF_RUMBLE:
		if (mlnx_is_emulated(mlnxeff))
			return mlnxeff->updated_at + update_rate_jiffies;
	case FF_DAMPER:
	case FF_FRICTION:
	case FF_INERTIA:
	case FF_SPRING:
	default:
		if (time_after_eq(mlnxeff->begin_at, now))
			return mlnxeff->begin_at;

		return mlnxeff->stop_at;
	}
}

static void mlnx_schedule_playback(struct mlnx_device *mlnxdev)
{
	struct mlnx_effect *mlnxeff;
	const unsigned long now = jiffies;
	int events = 0;
	int i;
	unsigned long earliest = 0;
	unsigned long time;

	/* Iterate over all effects and determine the earliest
	 * time when we have to attend to any */
	for (i = 0; i < FF_MAX_EFFECTS; i++) {
		mlnxeff = &mlnxdev->effects[i];

		if (!mlnx_is_started(mlnxeff))
			continue; /* Effect is not started, skip it */

		if (mlnx_is_playing(mlnxeff))
			time = mlnx_get_update_time(mlnxeff,
						mlnxdev->update_rate_jiffies);
		else
			time = mlnxeff->begin_at;

		pr_debug("Update time for effect %d: %lu\n", i, time);

		/* Scheduled time is in the future and is either
		 * before the current earliest time or it is
		 * the first valid time value in this pass */
		if (time_before_eq(now, time) &&
		    (++events == 1 || time_before(time, earliest)))
			earliest = time;
	}

	if (events) {
		pr_debug("Events: %d, earliest: %lu\n", events, earliest);
		mod_timer(&mlnxdev->timer, earliest);
	} else {
		pr_debug("No events, deactivating timer\n");
		del_timer(&mlnxdev->timer);
	}
}

static u16 mlnx_calculate_rumble_direction(const u32 total_mag, const u16 total_dir,
					   const u32 new_mag, const u16 new_dir)
{
	if (!new_mag)
		return total_dir;
	if (!total_mag)
		return new_dir;
	return (((total_dir >> 1) * total_mag +
		(new_dir >> 1) * new_mag) /
		(total_mag + new_mag)) << 1;
}

static void mlnx_add_force(struct mlnx_effect *mlnxeff, s32 *cfx, s32 *cfy,
			   const u16 gain)
{
	const struct ff_effect *effect = &mlnxeff->effect;
	u16 direction;
	s32 level;

	pr_debug("Processing effect type %d, ID %d\n",
		 mlnxeff->effect.type, mlnxeff->effect.id);

	direction = mlnxeff->effect.direction * 360 / 0xffff;
	pr_debug("Direction deg: %u\n", direction);

	switch (mlnxeff->effect.type) {
	case FF_CONSTANT:
		level = mlnx_apply_envelope(mlnxeff, effect->u.constant.level);
		break;
	case FF_PERIODIC:
		level = mlnx_apply_envelope(mlnxeff,
					    effect->u.periodic.magnitude);
		level = mlnx_calculate_periodic(mlnxeff, level);
		break;
	case FF_RAMP:
		level = mlnx_calculate_ramp(mlnxeff);
		break;
	default:
		pr_err("Effect %d not handled by mlnx_add_force\n",
		       mlnxeff->effect.type);
		return;
	}

	*cfx += mlnx_calculate_x_force(level, direction) * gain / 0xffff;
	*cfy += mlnx_calculate_y_force(level, direction) * gain / 0xffff;
}

static void mlnx_add_rumble(const struct mlnx_effect *mlnxeff, u32 *strong_mag,
			    u32 *weak_mag, u16 *strong_dir,
			    u16 *weak_dir, const u16 gain)
{
	const struct ff_effect *eff = &mlnxeff->effect;
	const struct ff_rumble_effect *reff = &mlnxeff->effect.u.rumble;
	const u32 new_strong_mag = (u32)reff->strong_magnitude * gain / 0xffffU;
	const u32 new_weak_mag = (u32)reff->weak_magnitude * gain / 0xffffU;

	*strong_dir = mlnx_calculate_rumble_direction(*strong_mag, *strong_dir,
						      new_strong_mag,
						      eff->direction);
	*weak_dir = mlnx_calculate_rumble_direction(*weak_mag, *weak_dir,
						    new_weak_mag,
						    eff->direction);
	*strong_mag += new_strong_mag;
	*weak_mag += new_weak_mag;
}

static void mlnx_add_emul_periodic(const struct mlnx_effect *mlnxeff,
				   u32 *strong_mag, u32 *weak_mag,
				   u16 *strong_dir, u16 *weak_dir,
				   const u16 gain)
{
	const struct ff_effect *eff = &mlnxeff->effect;
	const u32 level = (u32)abs(mlnx_apply_envelope(mlnxeff,
						  eff->u.periodic.magnitude)) * gain / 0x7fffU;

	*strong_dir = mlnx_calculate_rumble_direction(*strong_mag, *strong_dir,
						      level, eff->direction);
	*weak_dir = mlnx_calculate_rumble_direction(*weak_mag, *weak_dir,
						    level, eff->direction);

	*strong_mag += level;
	*weak_mag += level;
}

static void mlnx_add_emul_rumble(const struct mlnx_effect *mlnxeff, s32 *cfx,
				 s32 *cfy, const u16 gain,
				 const unsigned long now,
				 const unsigned long update_rate_jiffies)
{
	const struct ff_effect *effect = &mlnxeff->effect;
	const u16 strong = effect->u.rumble.strong_magnitude;
	const u16 weak = effect->u.rumble.weak_magnitude;
	/* To calculate 't', we pretend that mlnxeff->begin_at == 0, thus t == now.  */
	/* This will synchronise all simultaneously playing emul rumble effects,     */
	/* otherwise non-deterministic phase-inversions could occur depending on     */
	/* upload time, which could lead to undesired cancellation of these effects. */
	const unsigned long t = now % (4UL * update_rate_jiffies);
	s32 level = 0;
	bool direction_up;
	bool direction_left;

	if (strong)
		level += (strong / 4) * (t < 2UL * update_rate_jiffies ? 1 : -1);
	if (weak)
		level += (weak / 4) * (t < 2UL * update_rate_jiffies ?
					(t < 1UL * update_rate_jiffies ? 1 : -1) :
					(t < 3UL * update_rate_jiffies ? 1 : -1));
	direction_up = (effect->direction > 0x3fffU && effect->direction <= 0xbfffU);
	direction_left = (effect->direction <= 0x7fffU);

	pr_debug("Emulated cf: %d, t: %lu, n: %lu, begin: %lu, diff: %lu j: %lu\n",
		 level, t, now, mlnxeff->begin_at, now - mlnxeff->begin_at,
		 update_rate_jiffies);
	level = (level * gain) / 0xffff;
	*cfx += direction_left ? -level : level;
	*cfy += direction_up ? -level : level;
}

static void mlnx_play_effects(struct mlnx_device *mlnxdev)
{
	const u16 gain = mlnxdev->gain;
	const unsigned long now = jiffies;
	int i;
	s32 cfx = 0;
	s32 cfy = 0;
	u32 strong_mag = 0;
	u32 weak_mag = 0;
	u16 strong_dir = 0;
	u16 weak_dir = 0;

	for (i = 0; i < FF_MAX_EFFECTS; i++) {
		struct mlnx_effect *mlnxeff = &mlnxdev->effects[i];

		if (!mlnx_is_started(mlnxeff)) {
			pr_debug("Effect %hd/%d not started\n",
				 mlnxeff->effect.id, i);
			continue;
		}

		if (time_before(now, mlnxeff->begin_at)) {
			pr_debug("Effect %hd/%d begins at a later time\n",
				 mlnxeff->effect.id, i);
			continue;
		}

		if (time_before_eq(mlnxeff->stop_at, now) && mlnxeff->effect.replay.length) {
			pr_debug("Effect %hd/%d has to be stopped\n",
				 mlnxeff->effect.id, i);

			mlnx_clr_playing(mlnxeff);
			if (--mlnxeff->repeat > 0)
				mlnx_restart_effect(mlnxdev, mlnxeff);
			else {
				mlnx_stop_effect(mlnxdev, mlnxeff);
				mlnx_clr_started(mlnxeff);
				mlnx_clr_emulated(mlnxeff);
				if (mlnx_is_conditional(&mlnxeff->effect))
					mlnx_erase_conditional(mlnxdev, &mlnxeff->effect);
			}

			continue;
		}

		switch (mlnxeff->effect.type) {
		case FF_PERIODIC:
			if (mlnxdev->emul == EMUL_PERIODIC) {
				if (!mlnx_test_set_playing(mlnxeff)) {
					mlnxdev->rumble_playing++;
					pr_debug("Starting emul periodic, total rumble %u\n",
						 mlnxdev->rumble_playing);
				}
				__set_bit(FF_EFFECT_EMULATED, &mlnxeff->flags);
				mlnx_add_emul_periodic(mlnxeff, &strong_mag, &weak_mag,
						       &strong_dir, &weak_dir, gain);
				break;
			}
		case FF_CONSTANT:
		case FF_RAMP:
			if (!mlnx_test_set_playing(mlnxeff)) {
				mlnxdev->combinable_playing++;
				pr_debug("Starting combinable effect, total %u\n",
					 mlnxdev->combinable_playing);
			}
			mlnx_add_force(mlnxeff, &cfx, &cfy, gain);
			break;
		case FF_RUMBLE:
			if (mlnxdev->emul == EMUL_RUMBLE) {
				if (!mlnx_test_set_playing(mlnxeff)) {
					mlnxdev->combinable_playing++;
					pr_debug("Starting emul rumble, total comb %u\n",
						 mlnxdev->combinable_playing);
				}
				__set_bit(FF_EFFECT_EMULATED, &mlnxeff->flags);
				mlnx_add_emul_rumble(mlnxeff, &cfx, &cfy, gain, now,
						     mlnxdev->update_rate_jiffies);
			} else {
				if (!mlnx_test_set_playing(mlnxeff)) {
					mlnxdev->rumble_playing++;
					pr_debug("Starting rumble effect, total %u\n",
						 mlnxdev->rumble_playing);
				}
				mlnx_add_rumble(mlnxeff, &strong_mag, &weak_mag,
						&strong_dir, &weak_dir, gain);
			}
			break;
		case FF_DAMPER:
		case FF_FRICTION:
		case FF_INERTIA:
		case FF_SPRING:
			if (!mlnx_test_set_playing(mlnxeff)) {
				const struct mlnx_effect_command ecmd = {
					.cmd = MLNX_START_UNCOMB,
					.u.uncomb.id = i,
					.u.uncomb.effect = &mlnxeff->effect
				};
				mlnxdev->control_effect(mlnxdev->dev,
						      mlnxdev->private, &ecmd);
			}
			break;
		default:
			pr_debug("Unhandled type of effect\n");
		}
		mlnxeff->updated_at = now;
	}

	if (mlnxdev->combinable_playing) {
		const struct mlnx_effect_command ecmd = {
			.cmd = MLNX_START_COMBINED,
			.u.simple_force = {
				.x = clamp(cfx, -0x7fff, 0x7fff),
				.y = clamp(cfy, -0x7fff, 0x7fff)
			}
		};
		mlnxdev->control_effect(mlnxdev->dev, mlnxdev->private, &ecmd);
	}
	if (mlnxdev->rumble_playing) {
		const struct mlnx_effect_command ecmd = {
			.cmd = MLNX_START_RUMBLE,
			.u.rumble_force = {
				.strong = clamp(strong_mag, (u32)0, (u32)0xffffU),
				.weak = clamp(weak_mag, (u32)0, (u32)0xffffU),
				.strong_dir = clamp(strong_dir, (u16)0, (u16)0xffffU),
				.weak_dir = clamp(weak_dir, (u16)0, (u16)0xffffU)
			}
		};
		mlnxdev->control_effect(mlnxdev->dev, mlnxdev->private, &ecmd);
	}

	mlnx_schedule_playback(mlnxdev);
}

static void mlnx_set_gain(struct input_dev *dev, u16 gain)
{
	struct mlnx_device *mlnxdev = dev->ff->private;
	int i;

	mlnxdev->gain = gain;

	for (i = 0; i < FF_MAX_EFFECTS; i++) {
		struct mlnx_effect *eff = &mlnxdev->effects[i];
		if (eff == NULL)
			continue;
		if (mlnx_is_playing(eff)) {
			if (mlnx_is_combinable(&eff->effect)) {
				mlnx_clr_playing(eff);
				if (mlnx_is_emulated(eff))
					--mlnxdev->rumble_playing;
				else
					--mlnxdev->combinable_playing;
			} else if (mlnx_is_rumble(&eff->effect)) {
				mlnx_clr_playing(eff);
				if (mlnx_is_emulated(eff))
					--mlnxdev->combinable_playing;
				else
					--mlnxdev->rumble_playing;
			} else if (mlnx_is_conditional(&eff->effect)) {
				mlnx_upload_conditional(mlnxdev, eff);
			}
		}
	}

	mlnx_play_effects(mlnxdev);
}

static int mlnx_startstop(struct input_dev *dev, int effect_id, int repeat)
{
	struct mlnx_device *mlnxdev = dev->ff->private;
	struct mlnx_effect *mlnxeff = &mlnxdev->effects[effect_id];
	int ret;

	if (repeat > 0) {
		pr_debug("Starting effect ID %d\n", effect_id);
		mlnxeff->repeat = repeat;

		if (!mlnx_is_started(mlnxeff)) {
			/* Check that device has a free effect slot */
			if (mlnx_is_conditional(&mlnxeff->effect)) {
				ret = mlnx_upload_conditional(mlnxdev, mlnxeff);
				if (ret) {
					/* Device effect slots are all occupied */
					pr_debug("No free effect slot for EID %d\n", effect_id);
					return ret;
				}
			}
			mlnx_start_effect(mlnxeff);
		}
	} else {
		pr_debug("Stopping effect ID %d\n", effect_id);
		if (mlnx_is_started(mlnxeff)) {
			if (mlnx_is_playing(mlnxeff)) {
				mlnx_clr_playing(mlnxeff);
				mlnx_stop_effect(mlnxdev, mlnxeff);
			}
			mlnx_clr_started(mlnxeff);
			mlnx_clr_emulated(mlnxeff);

			if (mlnx_is_conditional(&mlnxeff->effect))
				return mlnx_erase_conditional(mlnxdev, &mlnxeff->effect);
		} else {
			pr_debug("Effect ID %d already stopped\n", effect_id);
			return 0;
		}
	}
	mlnx_play_effects(mlnxdev);

	return 0;
}

static void mlnx_timer_fired(unsigned long data)
{
	struct input_dev *dev = (struct input_dev *)data;
	unsigned long flags;

	spin_lock_irqsave(&dev->event_lock, flags);
	mlnx_play_effects(dev->ff->private);
	spin_unlock_irqrestore(&dev->event_lock, flags);
}

static int mlnx_upload(struct input_dev *dev, struct ff_effect *effect,
		       struct ff_effect *old)
{
	struct mlnx_device *mlnxdev = dev->ff->private;
	struct mlnx_effect *mlnxeff = &mlnxdev->effects[effect->id];
	const u16 length = effect->replay.length;
	const u16 delay = effect->replay.delay;
	int ret, fade_from;

effect->direction = 0x4000; /* TEMPORARY fix to test games that don't 
                               set direction properly for wheels. */

	/* Effect's timing is below kernel timer resolution */
	if (length && length < FF_MIN_EFFECT_LENGTH)
		effect->replay.length = FF_MIN_EFFECT_LENGTH;
	if (delay && delay < FF_MIN_EFFECT_LENGTH)
		effect->replay.delay = FF_MIN_EFFECT_LENGTH;

	/* Periodic effects must have a non-zero period */
	if (effect->type == FF_PERIODIC) {
		if (!effect->u.periodic.period)
			return -EINVAL;
	}
	/* Ramp effects cannot be infinite */
	if (effect->type == FF_RAMP && !length)
		return -EINVAL;

	if (mlnx_is_combinable(effect)) {
		const struct ff_envelope *envelope = mlnx_get_envelope(effect);

		/* Infinite effects cannot fade */
		if (!length && envelope->fade_length > 0)
			return -EINVAL;
		/* Fade length cannot be greater than effect duration */
		fade_from = (int)length - (int)envelope->fade_length;
		if (fade_from < 0)
			return -EINVAL;
		/* Envelope cannot start fading before it finishes attacking */
		if (fade_from < envelope->attack_length && fade_from > 0)
			return -EINVAL;
	}


	spin_lock_irq(&dev->event_lock);
	mlnxeff->effect = *effect; /* Keep internal copy of the effect */

	if (mlnx_is_conditional(effect)) {
		/* store base effect for applying gain */
		mlnxeff->base_effect = *effect;
	}	
	
	/* Check if the effect being modified is playing */
	if (mlnx_is_started(mlnxeff)) {
		if (mlnx_is_playing(mlnxeff)) {
			mlnx_clr_playing(mlnxeff);
			ret = mlnx_restart_effect(mlnxdev, mlnxeff);

			if (ret) {
				/* Restore the original effect */
				if (old)
					mlnxeff->effect = *old;
				spin_unlock_irq(&dev->event_lock);
				return ret;
			}
		}

		mlnx_schedule_playback(mlnxdev);
	}

	spin_unlock_irq(&dev->event_lock);

	return 0;
}

int input_ff_create_mlnx(struct input_dev *dev, void *data,
			 int (*control_effect)(struct input_dev *, void *, const struct mlnx_effect_command *),
			 const u16 update_rate)
{
	struct mlnx_device *mlnxdev;
	int ret;
	const u16 min_update_rate = update_rate < FF_MIN_EFFECT_LENGTH ?
				    FF_MIN_EFFECT_LENGTH : update_rate;

	mlnxdev = kzalloc(sizeof(*mlnxdev), GFP_KERNEL);
	if (!mlnxdev)
		return -ENOMEM;

	mlnxdev->dev = dev;
	mlnxdev->private = data;
	mlnxdev->control_effect = control_effect;
	mlnxdev->gain = 0xffff;
	mlnxdev->update_rate_jiffies = msecs_to_jiffies(min_update_rate);
	input_set_capability(dev, EV_FF, FF_GAIN);
	setup_timer(&mlnxdev->timer, mlnx_timer_fired, (unsigned long)dev);

	/* Set up effect emulation if needed */
	if (test_bit(FF_PERIODIC, dev->ffbit) &&
	    !test_bit(FF_RUMBLE, dev->ffbit)) {
		set_bit(FF_RUMBLE, dev->ffbit);
		mlnxdev->emul = EMUL_RUMBLE;
		pr_debug("Emulating RUMBLE with PERIODIC\n");
	} else if (test_bit(FF_RUMBLE, dev->ffbit) &&
		   !test_bit(FF_PERIODIC, dev->ffbit)) {
		set_bit(FF_PERIODIC, dev->ffbit);
		set_bit(FF_SINE, dev->ffbit);
		set_bit(FF_SQUARE, dev->ffbit);
		set_bit(FF_TRIANGLE, dev->ffbit);
		set_bit(FF_SAW_DOWN, dev->ffbit);
		set_bit(FF_SAW_UP, dev->ffbit);
		mlnxdev->emul = EMUL_PERIODIC;
		pr_debug("Emulating PERIODIC with RUMBLE\n");
	} else {
		mlnxdev->emul = EMUL_NOTHING;
		pr_debug("No effect emulation is necessary\n");
	}

	ret = input_ff_create(dev, FF_MAX_EFFECTS);
	if (ret) {
		kfree(mlnxdev);
		return ret;
	}


	dev->ff->private = mlnxdev;
	dev->ff->upload = mlnx_upload;
	dev->ff->set_gain = mlnx_set_gain;
	dev->ff->destroy = mlnx_destroy;
	dev->ff->playback = mlnx_startstop;

	pr_debug("Device successfully registered.\n");
	return 0;
}
EXPORT_SYMBOL_GPL(input_ff_create_mlnx);
