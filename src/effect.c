//
// nazghul - an old-school RPG engine
// Copyright (C) 2004 Gordon McNutt
//
// This program is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 2 of the License, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
// more details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
// Suite 330, Boston, MA 02111-1307 USA
//
// Gordon McNutt
// gmcnutt@users.sourceforge.net
//

#include "effect.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

char *EFFECT_ID = "EFFECT";

struct effect *effect_new(char *tag, scheme *sc, pointer exec_proc,
                          pointer apply_proc, pointer rm_proc,
                          char *name, char *description)
{
        struct effect *et;

        et = (struct effect*)calloc(1, sizeof(*et));
        assert(et);

        et->ID = EFFECT_ID;

        if (exec_proc) {
                et->exec = closure_new(sc, exec_proc);
                closure_ref(et->exec);
        }

        if (apply_proc) {
                et->apply = closure_new(sc, apply_proc);
                closure_ref(et->apply);
        }

        if (rm_proc) {
                et->rm = closure_new(sc, rm_proc);
                closure_ref(et->rm);
        }

        et->tag = strdup(tag);
        assert(et->tag);

        et->name = strdup(name);
        assert(et->name);

        et->description = strdup(description);
        assert(et->description);

        return et;
}

extern void effect_del(struct effect *et)
{
        free(et->name);
        free(et->description);
        if (et->exec)
                closure_unref(et->exec);
        if (et->apply)
                closure_unref(et->apply);
        if (et->rm)
                closure_unref(et->rm);
        free(et);
}
