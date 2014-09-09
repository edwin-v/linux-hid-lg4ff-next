/*
 * Tests the force feedback driver
 * Copyright 2001 Johann Deneux <deneux@ifrance.com>
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
 *
 * You can contact the author by email at this address:
 * Johann Deneux <deneux@ifrance.com>
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <linux/input.h>
#include <unistd.h>
#include <string.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <stdlib.h>

#define FF_GAIN_PERIODIC	0x71
#define FF_GAIN_CONSTANT	0x72
#define FF_GAIN_SPRING		0x73
#define FF_GAIN_FRICTION	0x74
#define FF_GAIN_DAMPER		0x75
#define FF_GAIN_INERTIA		0x76

void send_gain_effect(int fd, int type, int value)
{
	if (value < 0 || value > 150) return;
	
	value = 0xaaaa * value / 100;
	
	struct ff_effect eff;
	eff.type = FF_INERTIA;
	eff.id = -1;
	eff.u.condition[0].left_saturation = type;
	eff.u.condition[0].right_saturation = value;
	eff.u.condition[0].right_coeff = 0x2000;
	eff.u.condition[0].left_coeff = 0x2000;
	eff.u.condition[0].deadband = 0x0;
	eff.u.condition[0].center = 0x0;
	eff.u.condition[1] = eff.u.condition[0];
	eff.trigger.button = 0;
	eff.trigger.interval = 0;
	eff.replay.length = 20000; /* 20 seconds */
	eff.replay.delay = 0;
	
	ioctl(fd, EVIOCSFF, &eff); // ignore return, it should give an error
}

int main(int argc, char** argv)
{
	int fd;
	char device_file_name[64];
	int i;
	int gain = -1;
	int gain_periodic = -1;
	int gain_constant = -1;
	int gain_spring = -1;
	int gain_friction = -1;
	int gain_damper = -1;
	int gain_inertia = -1;
	int autocenter = -1;

	strncpy(device_file_name, "/dev/input/event0", 64);

	for (i=1; i<argc; ++i) {
		if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
			printf("Usage: %s /dev/input/eventXX [-g gain] [-a autocenter_strength]\n", argv[0]);
			printf("                      [-gp gain_periodic_forces] [-gc gain_constant_forces]\n");
			printf("                      [-gs gain_spring_forces] [-gf gain_friction_forces]\n");
			printf("                      [-gd gain_damper_forces] [-gi gain_inertia_forces]\n\n");
			printf("Sets the gain and the autocenter of a force-feedback device\n");
			printf("Values should be between 0 and 150, with 100 the default (unchanged) value.\n");
			exit(1);
		}
		else if (strcmp(argv[i], "-g") == 0) {
			if (++i >= argc) {
				fprintf(stderr, "Missing gain value\n");
				exit(1);
			}
			gain = atoi(argv[i]);
		}
		else if (strcmp(argv[i], "-a") == 0) {
			if (++i >= argc) {
				fprintf(stderr, "Missing auto-center value\n");
				exit(1);
			}
			autocenter = atoi(argv[i]);
		}
		else if (strcmp(argv[i], "-gp") == 0) {
			if (++i >= argc) {
				fprintf(stderr, "Missing gain value for periodic forces\n");
				exit(1);
			}
			gain_periodic = atoi(argv[i]);
		}
		else if (strcmp(argv[i], "-gc") == 0) {
			if (++i >= argc) {
				fprintf(stderr, "Missing gain value for constant forces\n");
				exit(1);
			}
			gain_constant = atoi(argv[i]);
		}
		else if (strcmp(argv[i], "-gs") == 0) {
			if (++i >= argc) {
				fprintf(stderr, "Missing gain value for spring forces\n");
				exit(1);
			}
			gain_spring = atoi(argv[i]);
		}
		else if (strcmp(argv[i], "-gf") == 0) {
			if (++i >= argc) {
				fprintf(stderr, "Missing gain value for friction forces\n");
				exit(1);
			}
			gain_friction = atoi(argv[i]);
		}
		else if (strcmp(argv[i], "-gd") == 0) {
			if (++i >= argc) {
				fprintf(stderr, "Missing gain value for damper forces\n");
				exit(1);
			}
			gain_damper = atoi(argv[i]);
		}
		else if (strcmp(argv[i], "-gi") == 0) {
			if (++i >= argc) {
				fprintf(stderr, "Missing gain value for inertia forces\n");
				exit(1);
			}
			gain_inertia = atoi(argv[i]);
		}
		else {
			strncpy(device_file_name, argv[i], 64);
		}
	}

	if (autocenter == -1 && gain == -1 && gain_periodic == -1 && 
	    gain_constant == -1 && gain_spring == -1 && gain_friction == -1 && 
	    gain_damper == -1 && gain_inertia == -1) {
		exit(0);
	}

	/* Open device */
	fd = open(device_file_name, O_RDWR);
	if (fd == -1) {
		perror("Open device file");
		exit(1);
	}
	printf("Device %s opened\n", device_file_name);

	if (autocenter >= 0 && autocenter <= 100) {
		struct input_event ie;
		ie.type = EV_FF;
		ie.code = FF_AUTOCENTER;
		ie.value = 0xFFFFUL * autocenter / 100;
		if (write(fd, &ie, sizeof(ie)) == -1)
			perror("set auto-center");
	}

	if (gain >= 0 && gain <= 150) {
		struct input_event ie;
		ie.type = EV_FF;
		ie.code = FF_GAIN;
		ie.value = 0xAAAAUL * gain / 100;
		if (write(fd, &ie, sizeof(ie)) == -1)
			perror("set gain");
	}
	
	send_gain_effect(fd, FF_GAIN_PERIODIC, gain_periodic);
	send_gain_effect(fd, FF_GAIN_CONSTANT, gain_constant);
	send_gain_effect(fd, FF_GAIN_SPRING, gain_spring);
	send_gain_effect(fd, FF_GAIN_FRICTION, gain_friction);
	send_gain_effect(fd, FF_GAIN_DAMPER, gain_damper);
	send_gain_effect(fd, FF_GAIN_INERTIA, gain_inertia);

	exit(0);
}
