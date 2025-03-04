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

#ifndef terrain_h
#define terrain_h

#include "list.h"
#include "Object.h"
#include "ptable.h"		/* for ptable_get() */
#include <stdio.h>		/* for FILE */
#include <closure.h>

struct terrain {
	struct list session_list;/* list of all terrains in this session */
	char *tag;
	char *name;
	struct sprite *sprite;
	struct terrain_map *combat_map;
	unsigned char alpha;
	int pclass;
	int light;
	int permeable;
	closure_t *effect;	/* when stood on */
	closure_t *renderCombat;	/* closure to set up combat map */
};

/**
 * Constructor and deref.
 */
struct terrain *terrain_new(
        const char *tag, const char *name, struct sprite *sprite, int pclass);
void terrain_deref(struct terrain *terrain);

/**
 * Returns a pointer to the wilderness combat map associated with the terrain,
 * if any. Warning: this pointer is not ref-counted.
 */
struct terrain_map *terrain_get_combat_map(struct terrain *terrain);

/**
 * Returns the passability class of the terrain.
 */
int terrain_get_pclass(struct terrain *terrain);


struct terrain_palette_entry {
	struct list lookup_list;	/* listed by fast lookup order */
	struct list edit_list;	/* listed by palette order */
	char *glyph;
	struct terrain *terrain;
};

void palette_entry_print(FILE * fp, int indent,
			 struct terrain_palette_entry *entry);

struct terrain_palette {
	struct list list;	/* list of palettes */
	char *tag;
	int widest_glyph;
	int current_terrain_index;
	int free_index;
	int num_entries;
	struct list lookup_head;	/* list of terrains for lookup */
	struct list edit_head;	/* list of terrains for editor */
};

struct terrain_palette *palette_contains_terrain(struct terrain_palette *pp,
						 struct terrain *tt);
struct terrain_palette *terrain_palette_new(const char *tag);
void terrain_palette_del(struct terrain_palette *pal);
void terrain_palette_add(struct terrain_palette *pal, char *glyph,
			 struct terrain *ter);
char *palette_glyph(struct terrain_palette *pp, int n);
char *palette_glyph_for_terrain(struct terrain_palette *pp, struct terrain *tt);
struct terrain_palette_entry *palette_entry(struct terrain_palette *palette,
					    int n);
struct terrain *palette_terrain(struct terrain_palette *pp, int n);

/**
 * Return the terrain matching `glyph` in `pp`.
 */
struct terrain *palette_terrain_for_glyph(
	struct terrain_palette *pp,
	char *glyph);

/**
 * Return zero unless `glyph` matches a NULL terrain in `palette`.
 *
 * If `glyph` is not found, or matches a non-NULL terrain, return
 * non-zero. This is to support blank sections of a terrain map. A special
 * glyph can be assigned to be a null terrain, which leaves a blank spot
 * wherever it appears.
 */
int palette_terrain_is_null_glyph(
	struct terrain_palette *palette,
	char *glyph);

struct terrain *palette_current_terrain(struct terrain_palette *pp);
void palette_print(FILE * fp, int indent, struct terrain_palette *pp);


#endif
