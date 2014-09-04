/*
 *  Force feedback support for Logitech Gaming Wheels
 *
 *  Including G27, G25, DFP, DFGT, FFEX, Momo, Momo2 &
 *  Speed Force Wireless (WiiWheel)
 *
 *  Copyright (c) 2010 Simon Wood <simon@mungewell.org>
 *  Copyright (c) 2014 Edwin Velds <e.velds@gmail.com>
 */

/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */


#include <linux/input.h>
#include <linux/usb.h>
#include <linux/hid.h>

#include "usbhid/usbhid.h"
#include "hid-lg.h"
#include "hid-ids.h"

#define to_hid_device(pdev) container_of(pdev, struct hid_device, dev)

#define LG4FF_MAX_EFFECTS 4
#define LG4FF_FFEX_BCDDEVICE 0x2100

#define CMD_DOWNLOAD_FORCE	0
#define CMD_PLAY_FORCE		2
#define CMD_STOP_FORCE		3
#define CMD_REFRESH_FORCE	12


static void hid_lg4ff_set_range_dfp(struct hid_device *hid, u16 range);
static void hid_lg4ff_set_range_g25(struct hid_device *hid, u16 range);
static ssize_t lg4ff_range_show(struct device *dev, struct device_attribute *attr, char *buf);
static ssize_t lg4ff_range_store(struct device *dev, struct device_attribute *attr, const char *buf, size_t count);

static DEVICE_ATTR(range, S_IRWXU | S_IRWXG | S_IROTH, lg4ff_range_show, lg4ff_range_store);

struct lg4ff_hw_effect_slot {
	bool is_playing;
	__s16 effect_id;
	__u8 last_cmd[7];
};

struct lg4ff_device_entry {
	__u32 product_id;
	__u16 range;
	__u16 min_range;
	__u16 max_range;
	struct lg4ff_hw_effect_slot hw_slots[4];
#ifdef CONFIG_LEDS_CLASS
	__u8  led_state;
	struct led_classdev *led[5];
#endif
	struct list_head list;
	void (*set_range)(struct hid_device *hid, u16 range);
};

static const signed short lg4ff_wheel_effects[] = {
	FF_CONSTANT,
	FF_SPRING,
	FF_DAMPER,
	FF_FRICTION,
	FF_AUTOCENTER,
	-1
};

struct lg4ff_mode_switch_cmd {
	const __u8 cmd_count;	/* Number of commands to send */
	const __u8 *cmd[];
};

struct lg4ff_emulated_wheel_mode {
	const int tag;
	const __u32 pid;
	const struct lg4ff_mode_switch_cmd *cmd;
};

struct lg4ff_mode_switcher {
	const u16 bcdDevice;
	const u16 mask;
	const __u32 native_pid;
	const __u32 *nonnative_pids;
	const struct lg4ff_mode_switch_cmd *native_cmds;
	const struct lg4ff_emulated_wheel_mode **emulated_modes;
};

struct lg4ff_wheel {
	const __u32 product_id;
	const signed short *ff_effects;
	const __u16 min_range;
	const __u16 max_range;
	void (*set_range)(struct hid_device *hid, u16 range);
};

static const struct lg4ff_wheel lg4ff_devices[] = {
	{USB_DEVICE_ID_LOGITECH_WHEEL,       lg4ff_wheel_effects, 40, 270, NULL},
	{USB_DEVICE_ID_LOGITECH_MOMO_WHEEL,  lg4ff_wheel_effects, 40, 270, NULL},
	{USB_DEVICE_ID_LOGITECH_DFP_WHEEL,   lg4ff_wheel_effects, 40, 900, hid_lg4ff_set_range_dfp},
	{USB_DEVICE_ID_LOGITECH_G25_WHEEL,   lg4ff_wheel_effects, 40, 900, hid_lg4ff_set_range_g25},
	{USB_DEVICE_ID_LOGITECH_DFGT_WHEEL,  lg4ff_wheel_effects, 40, 900, hid_lg4ff_set_range_g25},
	{USB_DEVICE_ID_LOGITECH_G27_WHEEL,   lg4ff_wheel_effects, 40, 900, hid_lg4ff_set_range_g25},
	{USB_DEVICE_ID_LOGITECH_MOMO_WHEEL2, lg4ff_wheel_effects, 40, 270, NULL},
	{USB_DEVICE_ID_LOGITECH_WII_WHEEL,   lg4ff_wheel_effects, 40, 270, NULL}
};


static const u8 lg4ff_go_native_dfp_cmd[] = {0xf8, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00};
static const u8 lg4ff_go_native_g25_cmd[] = {0xf8, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00};

static const u8 lg4ff_no_compat_on_usb_reset_cmd[] = {0xf8, 0x0a, 0x00, 0x00, 0x00, 0x00, 0x00};

static const u8 lg4ff_force_dfp_cmd[] = {0xf8, 0x09, 0x01, 0x01, 0x00, 0x00, 0x00};
static const u8 lg4ff_force_g25_cmd[] = {0xf8, 0x09, 0x02, 0x01, 0x00, 0x00, 0x00};
static const u8 lg4ff_force_dfgt_cmd[] = {0xf8, 0x09, 0x03, 0x01, 0x00, 0x00, 0x00};
static const u8 lg4ff_force_g27_cmd[] = {0xf8, 0x09, 0x04, 0x01, 0x00, 0x00, 0x00};

static const struct lg4ff_mode_switch_cmd lg4ff_switch_native_dfp_cmd = {
	1,
	{ lg4ff_go_native_dfp_cmd }
};

static const struct lg4ff_mode_switch_cmd lg4ff_switch_native_dfgt_cmd = {
	2,
	{ lg4ff_no_compat_on_usb_reset_cmd,
	  lg4ff_force_dfgt_cmd }
};

static const struct lg4ff_mode_switch_cmd lg4ff_switch_native_g25_cmd = {
	1,
	{ lg4ff_go_native_g25_cmd }
};

static const struct lg4ff_mode_switch_cmd lg4ff_switch_native_g27_cmd = {
	2,
	{ lg4ff_no_compat_on_usb_reset_cmd,
	  lg4ff_force_g27_cmd }
};

static const struct lg4ff_mode_switch_cmd lg4ff_switch_emulate_dfp_cmd = {
	1,
	{ lg4ff_force_dfp_cmd }
};

static const struct lg4ff_mode_switch_cmd lg4ff_switch_emulate_g25_cmd = {
	1,
	{ lg4ff_force_g25_cmd }
};

static const struct lg4ff_emulated_wheel_mode lg4ff_emulated_dfp_mode = {
	LG4FF_MSW_DFP,
	USB_DEVICE_ID_LOGITECH_DFP_WHEEL,
	&lg4ff_switch_emulate_dfp_cmd
};

static const struct lg4ff_emulated_wheel_mode lg4ff_emulated_g25_mode = {
	LG4FF_MSW_G25,
	USB_DEVICE_ID_LOGITECH_G25_WHEEL,
	&lg4ff_switch_emulate_g25_cmd
};

static const __u32 lg4ff_nonnative_pids_ffex_dfp[] = { USB_DEVICE_ID_LOGITECH_WHEEL, USB_DEVICE_ID_LOGITECH_DFP_WHEEL, 0 };
static const __u32 lg4ff_nonnative_pids_ffex_dfp_g25[] = { USB_DEVICE_ID_LOGITECH_WHEEL, USB_DEVICE_ID_LOGITECH_DFP_WHEEL,
						    USB_DEVICE_ID_LOGITECH_G25_WHEEL, 0 };
static const __u32 lg4ff_nonnative_pids_ffex[] = { USB_DEVICE_ID_LOGITECH_WHEEL, 0 };

static const struct lg4ff_emulated_wheel_mode *lg4ff_emulated_modes_dfp[] = { &lg4ff_emulated_dfp_mode, NULL };
static const struct lg4ff_emulated_wheel_mode *lg4ff_emulated_modes_dfp_g25[] = { &lg4ff_emulated_dfp_mode, &lg4ff_emulated_g25_mode, NULL };
static const struct lg4ff_emulated_wheel_mode *lg4ff_emulated_modes_none[] = { NULL };

static const struct lg4ff_mode_switcher lg4ff_mode_switchers[] = {
	/* DFGT */
	{0x1300, 0xff00, USB_DEVICE_ID_LOGITECH_DFGT_WHEEL, lg4ff_nonnative_pids_ffex_dfp, &lg4ff_switch_native_dfgt_cmd, lg4ff_emulated_modes_dfp},
	/* G27 */
	{0x1230, 0xfff0, USB_DEVICE_ID_LOGITECH_G27_WHEEL, lg4ff_nonnative_pids_ffex_dfp_g25, &lg4ff_switch_native_g27_cmd, lg4ff_emulated_modes_dfp_g25},
	/* G25 */
	{0x1200, 0xff00, USB_DEVICE_ID_LOGITECH_G25_WHEEL, lg4ff_nonnative_pids_ffex_dfp, &lg4ff_switch_native_g25_cmd, lg4ff_emulated_modes_dfp},
	/* DFP */
	{0x1000, 0xf000, USB_DEVICE_ID_LOGITECH_DFP_WHEEL, lg4ff_nonnative_pids_ffex, &lg4ff_switch_native_dfp_cmd, lg4ff_emulated_modes_none}
};

/* Recalculates X axis value accordingly to currently selected range */
static __s32 lg4ff_adjust_dfp_x_axis(__s32 value, __u16 range)
{
	__u16 max_range;
	__s32 new_value;

	if (range == 900)
		return value;
	else if (range == 200)
		return value;
	else if (range < 200)
		max_range = 200;
	else
		max_range = 900;

	new_value = 8192 + mult_frac(value - 8192, max_range, range);
	if (new_value < 0)
		return 0;
	else if (new_value > 16383)
		return 16383;
	else
		return new_value;
}

int lg4ff_adjust_input_event(struct hid_device *hid, struct hid_field *field,
			     struct hid_usage *usage, __s32 value, struct lg_drv_data *drv_data)
{
	struct lg4ff_device_entry *entry = drv_data->device_props;
	__s32 new_value = 0;

	if (!entry) {
		hid_err(hid, "Device properties not found");
		return 0;
	}

	switch (entry->product_id) {
	case USB_DEVICE_ID_LOGITECH_DFP_WHEEL:
		switch (usage->code) {
		case ABS_X:
			new_value = lg4ff_adjust_dfp_x_axis(value, entry->range);
			input_event(field->hidinput->input, usage->type, usage->code, new_value);
			return 1;
		default:
			return 0;
		}
	default:
		return 0;
	}
}

static struct lg4ff_device_entry *lg4ff_get_device_entry(struct input_dev *dev)
{
	struct hid_device *hid = input_get_drvdata(dev);
	struct lg4ff_device_entry *entry;
	struct lg_drv_data *drv_data;

	drv_data = hid_get_drvdata(hid);
	if (!drv_data) {
		hid_err(hid, "Private driver data not found!\n");
		return NULL;
	}

	entry = drv_data->device_props;
	if (!entry) {
		hid_err(hid, "Device properties not found!\n");
		return NULL;
	}
	return entry;
}

static __s8 lg4ff_get_slot(struct lg4ff_device_entry *entry, __s16 effect_id)
{
	int i;
	
	printk(KERN_DEBUG "Looking for effect %i, ", effect_id);
	for (i = 0; i < 4; i++)
		if (entry->hw_slots[i].effect_id == effect_id) {
			printk(KERN_DEBUG "found slot %i.\n", i);
			return i;
		}
	printk(KERN_DEBUG "found nothing!\n");
	return -1;
}

static int lg4ff_send_command(struct input_dev *dev, int slot, const __u8 *cmd)
{
	struct hid_device *hid = input_get_drvdata(dev);
	struct list_head *report_list = &hid->report_enum[HID_OUTPUT_REPORT].report_list;
	struct hid_report *report = list_entry(report_list->next, struct hid_report, list);
	__s32 *value = report->field[0]->value;
	struct lg4ff_device_entry *entry = lg4ff_get_device_entry(dev);
	int i, n = 0;
	
	/* check valid device entry */
	if (entry == NULL)
		return -EINVAL;
	
	for (i=0; i<7; i++) {
		if (entry->hw_slots[slot].last_cmd[i] == cmd[i])
			n++;
		value[i] = cmd[i];
	}
	
	if (n==7)
		return 0;
	
	memcpy(entry->hw_slots[slot].last_cmd, cmd, 7);
	
	hid_hw_request(hid, report, HID_REQ_SET_REPORT);
	
	return  0;
}

static int lg4ff_upload_effect(struct input_dev *dev,
                               struct ff_effect *effect, struct ff_effect *old)
{
	struct lg4ff_device_entry *entry = lg4ff_get_device_entry(dev);
	__u8 c[7];
	__s8 cmd, slot;
	int x, y, cr, cl;

	/* check valid device entry */
	if (entry == NULL)
		return -EINVAL;

	/* find existing hardware slot */
	slot = lg4ff_get_slot(entry, effect->id);

	if (slot >= 0) {
		// effect exists
		if (entry->hw_slots[slot].is_playing)
			cmd = CMD_REFRESH_FORCE; // replace effect
		else
			cmd = CMD_DOWNLOAD_FORCE; // upload effect
	} else {
		// get new effect slot
		slot = lg4ff_get_slot(entry, -1);

		if (slot < 0)
			return -ENOSPC;

		entry->hw_slots[slot].effect_id = effect->id;
		cmd = CMD_DOWNLOAD_FORCE;
	}


	c[0] = (16 << slot) | cmd;

	switch (effect->type) {
		case FF_CONSTANT:
			printk(KERN_DEBUG "Wheel constant: %i, direction %u\n", effect->u.constant.level * 0xff / 0xffff, effect->direction);
			x = 0x80 - effect->u.constant.level * 0xff / 0xffff;
			c[1] = 0x00;
			c[2] = x;
			c[3] = x;
			c[4] = x;
			c[5] = x;
			c[6] = 0;
			break;
		case FF_DAMPER:
			printk(KERN_DEBUG "Wheel damper: %i %i, sat %i %i\n", effect->u.condition[0].right_coeff
			                                                    , effect->u.condition[0].left_coeff
			                                                    , effect->u.condition[0].right_saturation
			                                                    , effect->u.condition[0].left_saturation);

			/* calculate damper force values */
			x = max(effect->u.condition[0].right_coeff / 2048, -15);
			y = max(effect->u.condition[0].left_coeff / 2048, -15);
			c[1] = 0x0C;
			c[2] = abs(y);
			c[3] = y < 0;
			c[4] = abs(x);
			c[5] = x < 0;
			// saturation only supported on DFP and newer
			c[6] = max(effect->u.condition[0].right_saturation / 256, 2);
			break;
		case FF_SPRING:
			printk(KERN_DEBUG "Wheel spring coef: %i % i, sat: %i center: %i deadband: %i\n",
			                   effect->u.condition[0].right_coeff,
			                   effect->u.condition[0].left_coeff,
			                   effect->u.condition[0].right_saturation,
			                   effect->u.condition[0].center,
			                   effect->u.condition[0].deadband);

			/* calculate offsets */
			x = clamp(effect->u.condition[0].center - effect->u.condition[0].deadband/2 + 0x8000, 0, 0xffff) >> 5;
			y = clamp(effect->u.condition[0].center + effect->u.condition[0].deadband/2 + 0x8000, 0, 0xffff) >> 5;
			printk(KERN_DEBUG "offsets %x %x\n", x, y);
			/* calculate coefs */
			cr = max(effect->u.condition[0].right_coeff / 2048, -15);
			cl = max(effect->u.condition[0].left_coeff / 2048, -15);
			c[1] = 0x0B;
			c[2] = x >> 3;
			c[3] = y >> 3;
			c[4] = 16 * abs(cr) + abs(cl);
			c[5] = (x & 7) * 32 + (cr & 16) + (y & 7) * 2 + (cl < 0);
			c[6] = max(effect->u.condition[0].right_saturation / 256, 1);
			break;
		case FF_FRICTION:
			printk(KERN_DEBUG "Wheel friction: %i %i, sat %i %i\n", effect->u.condition[0].right_coeff
			                                                      , effect->u.condition[0].left_coeff
			                                                      , effect->u.condition[0].right_saturation
			                                                      , effect->u.condition[0].left_saturation);

			/* calculate friction force values */
			x = max(effect->u.condition[0].right_coeff / 256, -255);
			y = max(effect->u.condition[0].left_coeff / 256, -255);
			c[1] = 0x0E;
			c[2] = abs(y);
			c[3] = abs(x);
			c[4] = effect->u.condition[0].right_saturation / 256;
			c[5] = 16*(x < 0) + (y < 0);
			c[6] = 0;
			break;
		default:
			printk(KERN_DEBUG "Unexpected force type %i!", effect->type);
			return -EINVAL;
	}
	return lg4ff_send_command(dev, slot, c);
}

static int lg4ff_play_effect(struct input_dev *dev, int effect_id)
{
	struct lg4ff_device_entry *entry = lg4ff_get_device_entry(dev);
	__u8 c[7] = {0, 0, 0, 0, 0, 0, 0};
	__s8 slot;

	/* check valid device entry */
	if (entry == NULL)
		return -EINVAL;

	/* find hardware slot for effect */
	slot = lg4ff_get_slot(entry, effect_id);

	if (slot < 0)
		return -EINVAL;

	/* create play command */
	c[0] = (16 << slot) | CMD_PLAY_FORCE;

	entry->hw_slots[slot].is_playing = true;

	return lg4ff_send_command(dev, slot, c);
}

static int lg4ff_stop_effect(struct input_dev *dev, int effect_id)
{
	struct lg4ff_device_entry *entry = lg4ff_get_device_entry(dev);
	__u8 c[7] = {0, 0, 0, 0, 0, 0, 0};
	__s8 slot;

	/* check valid device entry */
	if (entry == NULL)
		return -EINVAL;

	/* find hardware slot for effect */
	slot = lg4ff_get_slot(entry, effect_id);
	
	if (slot < 0)
		return -EINVAL;

	/* create stop command */
	c[0] = (16 << slot) | CMD_STOP_FORCE;

	entry->hw_slots[slot].is_playing = false;

	return lg4ff_send_command(dev, slot, c);
}

static int lg4ff_playback(struct input_dev *dev, int effect_id, int value)
{
	if (value > 0)
		return lg4ff_play_effect(dev, effect_id);
	else
		return lg4ff_stop_effect(dev, effect_id);
}

static int lg4ff_erase_effect(struct input_dev *dev, int effect_id)
{
	struct lg4ff_device_entry *entry = lg4ff_get_device_entry(dev);
	__s8 slot;
	
	/* check valid device entry */
	if (entry == NULL)
		return -EINVAL;

	/* find hardware slot for effect */
	slot = lg4ff_get_slot(entry, effect_id);
	
	if (slot < 0)
		return -EINVAL;

	/* clear hardware slot */
	entry->hw_slots[slot].effect_id = -1;
	entry->hw_slots[slot].is_playing = false;

	return 0;
}

/* Sends default autocentering command compatible with
 * all wheels except Formula Force EX */
static void hid_lg4ff_set_autocenter_default(struct input_dev *dev, u16 magnitude)
{
	struct hid_device *hid = input_get_drvdata(dev);
	struct list_head *report_list = &hid->report_enum[HID_OUTPUT_REPORT].report_list;
	struct hid_report *report = list_entry(report_list->next, struct hid_report, list);
	__s32 *value = report->field[0]->value;
	__u32 expand_a, expand_b;
	struct lg4ff_device_entry *entry;
	struct lg_drv_data *drv_data;

	drv_data = hid_get_drvdata(hid);
	if (!drv_data) {
		hid_err(hid, "Private driver data not found!\n");
		return;
	}

	entry = drv_data->device_props;
	if (!entry) {
		hid_err(hid, "Device properties not found!\n");
		return;
	}

	/* De-activate Auto-Center */
	if (magnitude == 0) {
		value[0] = 0xf5;
		value[1] = 0x00;
		value[2] = 0x00;
		value[3] = 0x00;
		value[4] = 0x00;
		value[5] = 0x00;
		value[6] = 0x00;

		hid_hw_request(hid, report, HID_REQ_SET_REPORT);
		return;
	}

	if (magnitude <= 0xaaaa) {
		expand_a = 0x0c * magnitude;
		expand_b = 0x80 * magnitude;
	} else {
		expand_a = (0x0c * 0xaaaa) + 0x06 * (magnitude - 0xaaaa);
		expand_b = (0x80 * 0xaaaa) + 0xff * (magnitude - 0xaaaa);
	}

	/* Adjust for non-MOMO wheels */
	switch (entry->product_id) {
	case USB_DEVICE_ID_LOGITECH_MOMO_WHEEL:
	case USB_DEVICE_ID_LOGITECH_MOMO_WHEEL2:
		break;
	default:
		expand_a = expand_a >> 1;
		break;
	}

	value[0] = 0xfe;
	value[1] = 0x0d;
	value[2] = expand_a / 0xaaaa;
	value[3] = expand_a / 0xaaaa;
	value[4] = expand_b / 0xaaaa;
	value[5] = 0x00;
	value[6] = 0x00;

	hid_hw_request(hid, report, HID_REQ_SET_REPORT);

	/* Activate Auto-Center */
	value[0] = 0x14;
	value[1] = 0x00;
	value[2] = 0x00;
	value[3] = 0x00;
	value[4] = 0x00;
	value[5] = 0x00;
	value[6] = 0x00;

	hid_hw_request(hid, report, HID_REQ_SET_REPORT);
}

/* Sends autocentering command compatible with Formula Force EX */
static void hid_lg4ff_set_autocenter_ffex(struct input_dev *dev, u16 magnitude)
{
	struct hid_device *hid = input_get_drvdata(dev);
	struct list_head *report_list = &hid->report_enum[HID_OUTPUT_REPORT].report_list;
	struct hid_report *report = list_entry(report_list->next, struct hid_report, list);
	__s32 *value = report->field[0]->value;
	magnitude = magnitude * 90 / 65535;

	value[0] = 0xfe;
	value[1] = 0x03;
	value[2] = magnitude >> 14;
	value[3] = magnitude >> 14;
	value[4] = magnitude;
	value[5] = 0x00;
	value[6] = 0x00;

	hid_hw_request(hid, report, HID_REQ_SET_REPORT);
}

/* Sends command to set range compatible with G25/G27/Driving Force GT */
static void hid_lg4ff_set_range_g25(struct hid_device *hid, u16 range)
{
	struct list_head *report_list = &hid->report_enum[HID_OUTPUT_REPORT].report_list;
	struct hid_report *report = list_entry(report_list->next, struct hid_report, list);
	__s32 *value = report->field[0]->value;

	dbg_hid("G25/G27/DFGT: setting range to %u\n", range);

	value[0] = 0xf8;
	value[1] = 0x81;
	value[2] = range & 0x00ff;
	value[3] = (range & 0xff00) >> 8;
	value[4] = 0x00;
	value[5] = 0x00;
	value[6] = 0x00;

	hid_hw_request(hid, report, HID_REQ_SET_REPORT);
}

/* Sends commands to set range compatible with Driving Force Pro wheel */
static void hid_lg4ff_set_range_dfp(struct hid_device *hid, __u16 range)
{
	struct list_head *report_list = &hid->report_enum[HID_OUTPUT_REPORT].report_list;
	struct hid_report *report = list_entry(report_list->next, struct hid_report, list);
	int start_left, start_right, full_range;
	__s32 *value = report->field[0]->value;

	dbg_hid("Driving Force Pro: setting range to %u\n", range);

	/* Prepare "coarse" limit command */
	value[0] = 0xf8;
	value[1] = 0x00;	/* Set later */
	value[2] = 0x00;
	value[3] = 0x00;
	value[4] = 0x00;
	value[5] = 0x00;
	value[6] = 0x00;

	if (range > 200) {
		report->field[0]->value[1] = 0x03;
		full_range = 900;
	} else {
		report->field[0]->value[1] = 0x02;
		full_range = 200;
	}
	hid_hw_request(hid, report, HID_REQ_SET_REPORT);

	/* Prepare "fine" limit command */
	value[0] = 0x81;
	value[1] = 0x0b;
	value[2] = 0x00;
	value[3] = 0x00;
	value[4] = 0x00;
	value[5] = 0x00;
	value[6] = 0x00;

	if (range == 200 || range == 900) {	/* Do not apply any fine limit */
		hid_hw_request(hid, report, HID_REQ_SET_REPORT);
		return;
	}

	/* Construct fine limit command */
	start_left = (((full_range - range + 1) * 2047) / full_range);
	start_right = 0xfff - start_left;

	value[2] = start_left >> 4;
	value[3] = start_right >> 4;
	value[4] = 0xff;
	value[5] = (start_right & 0xe) << 4 | (start_left & 0xe);
	value[6] = 0xff;

	hid_hw_request(hid, report, HID_REQ_SET_REPORT);
}

static int lg4ff_switch_mode(struct hid_device *hid, const struct lg4ff_mode_switch_cmd *cmd)
{
	struct list_head *report_list = &hid->report_enum[HID_OUTPUT_REPORT].report_list;
	struct hid_report *report = list_entry(report_list->next, struct hid_report, list);
	__s32 *value = report->field[0]->value;
	int i;

	for (i = 0; i < cmd->cmd_count; i++) {
		const u8 *c = cmd->cmd[i];

		value[0] = c[0];
		value[1] = c[1];
		value[2] = c[2];
		value[3] = c[3];
		value[4] = c[4];
		value[5] = c[5];
		value[6] = c[6];

		hid_hw_request(hid, report, HID_REQ_SET_REPORT);
	}

	return 0;
}

/* Read current range and display it in terminal */
static ssize_t lg4ff_range_show(struct device *dev, struct device_attribute *attr, char *buf)
{
	struct hid_device *hid = to_hid_device(dev);
	struct lg4ff_device_entry *entry;
	struct lg_drv_data *drv_data;
	size_t count;

	drv_data = hid_get_drvdata(hid);
	if (!drv_data) {
		hid_err(hid, "Private driver data not found!\n");
		return 0;
	}

	entry = drv_data->device_props;
	if (!entry) {
		hid_err(hid, "Device properties not found!\n");
		return 0;
	}

	count = scnprintf(buf, PAGE_SIZE, "%u\n", entry->range);
	return count;
}

/* Set range to user specified value, call appropriate function
 * according to the type of the wheel */
static ssize_t lg4ff_range_store(struct device *dev, struct device_attribute *attr, const char *buf, size_t count)
{
	struct hid_device *hid = to_hid_device(dev);
	struct lg4ff_device_entry *entry;
	struct lg_drv_data *drv_data;
	__u16 range = simple_strtoul(buf, NULL, 10);

	drv_data = hid_get_drvdata(hid);
	if (!drv_data) {
		hid_err(hid, "Private driver data not found!\n");
		return 0;
	}

	entry = drv_data->device_props;
	if (!entry) {
		hid_err(hid, "Device properties not found!\n");
		return 0;
	}

	if (range == 0)
		range = entry->max_range;

	/* Check if the wheel supports range setting
	 * and that the range is within limits for the wheel */
	if (entry->set_range != NULL && range >= entry->min_range && range <= entry->max_range) {
		entry->set_range(hid, range);
		entry->range = range;
	}

	return count;
}

#ifdef CONFIG_LEDS_CLASS
static void lg4ff_set_leds(struct hid_device *hid, __u8 leds)
{
	struct list_head *report_list = &hid->report_enum[HID_OUTPUT_REPORT].report_list;
	struct hid_report *report = list_entry(report_list->next, struct hid_report, list);
	__s32 *value = report->field[0]->value;

	value[0] = 0xf8;
	value[1] = 0x12;
	value[2] = leds;
	value[3] = 0x00;
	value[4] = 0x00;
	value[5] = 0x00;
	value[6] = 0x00;
	hid_hw_request(hid, report, HID_REQ_SET_REPORT);
}

static void lg4ff_led_set_brightness(struct led_classdev *led_cdev,
			enum led_brightness value)
{
	struct device *dev = led_cdev->dev->parent;
	struct hid_device *hid = container_of(dev, struct hid_device, dev);
	struct lg_drv_data *drv_data = hid_get_drvdata(hid);
	struct lg4ff_device_entry *entry;
	int i, state = 0;

	if (!drv_data) {
		hid_err(hid, "Device data not found.");
		return;
	}

	entry = (struct lg4ff_device_entry *)drv_data->device_props;

	if (!entry) {
		hid_err(hid, "Device properties not found.");
		return;
	}

	for (i = 0; i < 5; i++) {
		if (led_cdev != entry->led[i])
			continue;
		state = (entry->led_state >> i) & 1;
		if (value == LED_OFF && state) {
			entry->led_state &= ~(1 << i);
			lg4ff_set_leds(hid, entry->led_state);
		} else if (value != LED_OFF && !state) {
			entry->led_state |= 1 << i;
			lg4ff_set_leds(hid, entry->led_state);
		}
		break;
	}
}

static enum led_brightness lg4ff_led_get_brightness(struct led_classdev *led_cdev)
{
	struct device *dev = led_cdev->dev->parent;
	struct hid_device *hid = container_of(dev, struct hid_device, dev);
	struct lg_drv_data *drv_data = hid_get_drvdata(hid);
	struct lg4ff_device_entry *entry;
	int i, value = 0;

	if (!drv_data) {
		hid_err(hid, "Device data not found.");
		return LED_OFF;
	}

	entry = (struct lg4ff_device_entry *)drv_data->device_props;

	if (!entry) {
		hid_err(hid, "Device properties not found.");
		return LED_OFF;
	}

	for (i = 0; i < 5; i++)
		if (led_cdev == entry->led[i]) {
			value = (entry->led_state >> i) & 1;
			break;
		}

	return value ? LED_FULL : LED_OFF;
}
#endif

static int lg4ff_switch_ext_compatibility(struct hid_device *hid, const struct lg4ff_mode_switcher *s, const int switch_force_mode,
					  const __u32 pid)
{
	int k = 0;
	const struct lg4ff_emulated_wheel_mode *emul;

	while ((emul = s->emulated_modes[k++]) != NULL) {
		if (emul->tag == switch_force_mode) {
			if (pid != emul->pid) {
				dbg_hid("Switching device to extended compatibility mode\n");
				return lg4ff_switch_mode(hid, emul->cmd);
			}
			dbg_hid("Device already is in requested extended compatibility mode\n");
			return 0;
		}
	}
	dbg_hid("This device does not support the enforced compatibility mode, leaving in FFEX mode\n");
	return 0;
}

static int lg4ff_try_mode_switch(struct hid_device *hid, const u16 bcdDevice, int switch_force_mode)
{
	const __u32 pid = hid->product;
	int i;

	if (switch_force_mode < LG4FF_MSW_MIN || switch_force_mode > LG4FF_MSW_MAX)
		switch_force_mode = LG4FF_MSW_NATIVE;
	if (switch_force_mode == LG4FF_MSW_DONTSWITCH) {
		dbg_hid("Leaving device as it is\n");
		return 0;
	}

	for (i = 0; i < ARRAY_SIZE(lg4ff_mode_switchers); i++) {
		const struct lg4ff_mode_switcher *s = &lg4ff_mode_switchers[i];
		int j = 0;
		__u32 nonnative_pid;

		if (s->bcdDevice != (bcdDevice & s->mask))
			continue;

		if (pid == s->native_pid) {
			if (switch_force_mode != LG4FF_MSW_NATIVE)
				return lg4ff_switch_ext_compatibility(hid, s, switch_force_mode, pid);
			dbg_hid("Device already is in its native mode\n");
			return 0;
		}

		/* Check into which mode we want to switch the device to */
		while ((nonnative_pid = s->nonnative_pids[j++]) != 0) {
			if (pid == nonnative_pid) {
				if (switch_force_mode == LG4FF_MSW_NATIVE) {
					dbg_hid("Switching device to native mode\n");
					return lg4ff_switch_mode(hid, s->native_cmds);
				}
				return lg4ff_switch_ext_compatibility(hid, s, switch_force_mode, pid);
			}
		}
	}

	return 0;
}

int lg4ff_init(struct hid_device *hid, const int switch_force_mode)
{
	struct hid_input *hidinput = list_entry(hid->inputs.next, struct hid_input, list);
	struct input_dev *dev = hidinput->input;
	struct lg4ff_device_entry *entry;
	struct lg_drv_data *drv_data;
	struct usb_device_descriptor *udesc;
	struct ff_device *ff;
	int error, i, j;
	u16 bcdDevice;

	/* Check that the report looks ok */
	if (!hid_validate_values(hid, HID_OUTPUT_REPORT, 0, 0, 7))
		return -1;

	/* Check what wheel has been connected */
	for (i = 0; i < ARRAY_SIZE(lg4ff_devices); i++) {
		if (hid->product == lg4ff_devices[i].product_id) {
			dbg_hid("Found compatible device, product ID %04X\n", lg4ff_devices[i].product_id);
			break;
		}
	}

	if (i == ARRAY_SIZE(lg4ff_devices)) {
		hid_err(hid, "Device is not supported by lg4ff driver. If you think it should be, consider reporting a bug to"
			     "LKML, Simon Wood <simon@mungewell.org> or Michal Maly <madcatxster@gmail.com>\n");
		return -1;
	}

	/* Attempt to switch wheel to native mode when applicable */
	udesc = &(hid_to_usb_dev(hid)->descriptor);
	if (!udesc) {
		hid_err(hid, "NULL USB device descriptor\n");
		return -1;
	}
	bcdDevice = le16_to_cpu(udesc->bcdDevice);
	error = lg4ff_try_mode_switch(hid, bcdDevice, switch_force_mode);
	if (error)
		return error;

	/* Set supported force feedback capabilities */
	for (j = 0; lg4ff_devices[i].ff_effects[j] >= 0; j++)
		set_bit(lg4ff_devices[i].ff_effects[j], dev->ffbit);


	error = input_ff_create(dev, LG4FF_MAX_EFFECTS);

	if (error)
		return error;

	ff = dev->ff;
	ff->upload = lg4ff_upload_effect;
	ff->erase = lg4ff_erase_effect;
	//ff->set_gain = lg4ff_set_gain;
	ff->playback = lg4ff_playback;

	/* Get private driver data */
	drv_data = hid_get_drvdata(hid);
	if (!drv_data) {
		hid_err(hid, "Cannot add device, private driver data not allocated\n");
		return -1;
	}

	/* Initialize device properties */
	entry = kzalloc(sizeof(struct lg4ff_device_entry), GFP_KERNEL);
	if (!entry) {
		hid_err(hid, "Cannot add device, insufficient memory to allocate device properties.\n");
		return -ENOMEM;
	}
	drv_data->device_props = entry;

	entry->product_id = lg4ff_devices[i].product_id;
	entry->min_range = lg4ff_devices[i].min_range;
	entry->max_range = lg4ff_devices[i].max_range;
	entry->set_range = lg4ff_devices[i].set_range;
	for (i = 0; i < 4; i++)
		entry->hw_slots[i].effect_id = -1;

	/* Check if autocentering is available and
	 * set the centering force to zero by default */
	if (test_bit(FF_AUTOCENTER, dev->ffbit)) {
		if (bcdDevice == LG4FF_FFEX_BCDDEVICE)	/* Formula Force EX does not seem to support hi-res autocentering */
			dev->ff->set_autocenter = hid_lg4ff_set_autocenter_ffex;
		else
			dev->ff->set_autocenter = hid_lg4ff_set_autocenter_default;

		dev->ff->set_autocenter(dev, 0);
	}

	/* Create sysfs interface */
	error = device_create_file(&hid->dev, &dev_attr_range);
	if (error)
		return error;
	dbg_hid("sysfs interface created\n");

	/* Set the maximum range to start with */
	entry->range = entry->max_range;
	if (entry->set_range != NULL)
		entry->set_range(hid, entry->range);

#ifdef CONFIG_LEDS_CLASS
	/* register led subsystem - G27 only */
	entry->led_state = 0;
	for (j = 0; j < 5; j++)
		entry->led[j] = NULL;

	if (lg4ff_devices[i].product_id == USB_DEVICE_ID_LOGITECH_G27_WHEEL) {
		struct led_classdev *led;
		size_t name_sz;
		char *name;

		lg4ff_set_leds(hid, 0);

		name_sz = strlen(dev_name(&hid->dev)) + 8;

		for (j = 0; j < 5; j++) {
			led = kzalloc(sizeof(struct led_classdev)+name_sz, GFP_KERNEL);
			if (!led) {
				hid_err(hid, "can't allocate memory for LED %d\n", j);
				goto err;
			}

			name = (void *)(&led[1]);
			snprintf(name, name_sz, "%s::RPM%d", dev_name(&hid->dev), j+1);
			led->name = name;
			led->brightness = 0;
			led->max_brightness = 1;
			led->brightness_get = lg4ff_led_get_brightness;
			led->brightness_set = lg4ff_led_set_brightness;

			entry->led[j] = led;
			error = led_classdev_register(&hid->dev, led);

			if (error) {
				hid_err(hid, "failed to register LED %d. Aborting.\n", j);
err:
				/* Deregister LEDs (if any) */
				for (j = 0; j < 5; j++) {
					led = entry->led[j];
					entry->led[j] = NULL;
					if (!led)
						continue;
					led_classdev_unregister(led);
					kfree(led);
				}
				goto out;	/* Let the driver continue without LEDs */
			}
		}
	}
out:
#endif
	hid_info(hid, "Force feedback support for Logitech Gaming Wheels\n");
	return 0;
}



int lg4ff_deinit(struct hid_device *hid)
{
	struct lg4ff_device_entry *entry;
	struct lg_drv_data *drv_data;

	device_remove_file(&hid->dev, &dev_attr_range);

	drv_data = hid_get_drvdata(hid);
	if (!drv_data) {
		hid_err(hid, "Error while deinitializing device, no private driver data.\n");
		return -1;
	}
	entry = drv_data->device_props;
	if (!entry) {
		hid_err(hid, "Error while deinitializing device, no device properties data.\n");
		return -1;
	}

#ifdef CONFIG_LEDS_CLASS
	{
		int j;
		struct led_classdev *led;

		/* Deregister LEDs (if any) */
		for (j = 0; j < 5; j++) {

			led = entry->led[j];
			entry->led[j] = NULL;
			if (!led)
				continue;
			led_classdev_unregister(led);
			kfree(led);
		}
	}
#endif

	/* Deallocate memory */
	kfree(entry);

	dbg_hid("Device successfully unregistered\n");
	return 0;
}
