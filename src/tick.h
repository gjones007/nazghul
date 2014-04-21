/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2002, 2003, 2014 Gordon McNutt
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA 02111-1307 USA
 *
 * Gordon McNutt
 * gmcnutt@users.sourceforge.net
 */


/* Start/stop the thread that generates tick events. */
void tick_start(long msecs);
void tick_kill(void);

/* Pause/resume tick event generation. */
void tick_pause(void);
void tick_run(void);

/* Increment the global tick counter. */
void tick_increment(void);

/* Add/remove a periodic callback to run on tick counter changes. Returns a
 * handle for tick_unwatch(). */
void *tick_watch(long period_msecs, void (*callback)(void *arg), void *arg);
void tick_unwatch(void *handle);
