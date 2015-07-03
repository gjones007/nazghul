/*
 * nazghul - an old-school RPG engine
 * Copyright (C) 2002, 2003 Gordon McNutt
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

#include "tick.h"
#include "list.h"
#include "mem.h"

#include <assert.h>
#include <SDL.h>
#include <SDL_thread.h>


#define TICK_EVENT  1           /* The most wonderful thing about being a Tigger is I'm
                                 * the only one. */


typedef struct {
        struct list list;
        void (*callback) (void *arg);
        void *arg;
        long period;
} tick_watcher_t;


static int tick_paused = 0;
static int tick_killed = 0;
static SDL_Thread *tick_thread = NULL;
static long ticks = 0;
static struct list tick_watchers = {
        &tick_watchers,
        &tick_watchers
};


static int tick_main(void *data)
{
        long msecs;
        SDL_Event tick_event;

        msecs = (long) data;
        tick_event.type = SDL_USEREVENT;
        tick_event.user.code = TICK_EVENT;

        while (!tick_killed) {
                SDL_Delay(msecs);
                if (!tick_paused)
                        SDL_PushEvent(&tick_event);
        }

        return 0;
}


void tick_start(long msecs)
{
        assert(!tick_thread);
        tick_paused = 0;
        tick_killed = 0;
        if (msecs > 0) {
                tick_thread = SDL_CreateThread(tick_main, (void *) msecs);
        }
}


void tick_kill(void)
{
        tick_killed = 1;
        if (tick_thread) {
                SDL_WaitThread(tick_thread, NULL);
        }
}


void tick_pause(void)
{
        tick_paused = 1;
}


void tick_run(void)
{
        tick_paused = 0;
}


void tick_increment(void)
{
        ticks += 1;

        /* Call the watchers that fall on this period. */
        struct list *elem = tick_watchers.next;
        while (elem != &tick_watchers) {
                tick_watcher_t *h = list_entry(elem, tick_watcher_t, list);
                elem = elem->next;
                if (!(ticks % h->period)) {
                        h->callback(h->arg);
                }
        }
}


void *tick_watch(long period_msecs, void (*callback) (void *arg), void *arg)
{
        tick_watcher_t *h = MEM_ALLOC_TYPE(tick_watcher_t, NULL);
        h->period = period_msecs;
        h->callback = callback;
        h->arg = arg;
        list_add(&tick_watchers, &h->list);
        return h;
}


void tick_unwatch(void *handle)
{
        tick_watcher_t *h = (tick_watcher_t *) handle;
        list_remove(&h->list);
        mem_deref(h);
}
