//
// nazghul - an old-school RPG engine
// Copyright (C) 2002, 2003 Gordon McNutt
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
#ifndef occ_h
#define occ_h

#include "list.h"
#include "closure.h"

struct typical_items {
	int prob;
	class ObjectType *type;
	int n_max;
};

struct occ {
	struct list list;
	char *tag;
	char *name;
	float magic;

        int hp_mod;   /* part of base hp contributed by occupation */
        int hp_mult;  /* additional hp per-level contributed by occupation  */
        int mp_mod;   /* similar, for mana */
        int mp_mult;  /* similar, for mana */

        int hit_mod;  /* unused */
        int def_mod;  /* unused */
        int dam_mod;  /* unused */
        int arm_mod;  /* unused */
        
	class ObjectType *container;

/* 	int n_arms; */
/* 	class ArmsType **arms; */

	int n_items;
	struct typical_items *items;

	int n_traps;
	closure_t *traps;

        int xpval; /* reward for killing this type */
};

extern struct occ *occ_new(char *tag,
                           char *name,
                           float magic,
                           int hp_mod,
                           int hp_mult,
                           int mp_mod,
                           int mp_mult,
                           int hit_mod,
                           int def_mod,
                           int dam_mod,
                           int arm_mod,
                           int n_arms, int n_items, int n_traps);

extern void occ_del(struct occ *occ);

#endif
