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
#include "terrain.h"
#include "debug.h"
#include "sprite.h"
#include "common.h"
#include "object.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

//int TERRAIN_MAGIC = 0xc01dbee3;

extern struct terrain *terrain_new(char *tag, 
                                   char *name,
                                   struct sprite *sprite,
                                   int pclass, 
                                   int alpha, 
                                   int light)
{
	struct terrain *terrain;

        terrain = new struct terrain;
        assert(terrain);
        memset(terrain, 0, sizeof(*terrain));

        terrain->magic         = TERRAIN_MAGIC;
        terrain->tag           = strdup(tag);
	terrain->name          = strdup(name);
        terrain->sprite        = sprite;
	terrain->pclass        = pclass;
	terrain->alpha         = alpha;
        terrain->light         = light;
	return terrain;
}

void terrain_del(struct terrain *terrain)
{
        if (terrain->tag)
                free(terrain->tag);
	if (terrain->name)
		free(terrain->name);
        if (terrain->effect)
                closure_del(terrain->effect);
        delete terrain;
}

#define BOGUS_MAX_SIZE 255	// Hack, should get a constant from somewhere
// LONGEST_TERRAIN_GLYPH would be appropriate for glyph_str...

void palette_entry_print(FILE * fp, int indent,
			 struct terrain_palette_entry *entry)
{
	static char glyph_str[BOGUS_MAX_SIZE + 1];
        static char   tag_str[BOGUS_MAX_SIZE + 1];
	assert(fp);
        assert(entry);

	snprintf(glyph_str, BOGUS_MAX_SIZE, "\"%s\"", entry->glyph);
        snprintf(tag_str,   BOGUS_MAX_SIZE, "%s;",    entry->terrain->name);

	INDENT;
        fprintf(fp, "%-6s %s\n", glyph_str, tag_str);
} // palette_entry_print()

struct terrain_palette *terrain_palette_new(char *tag)
{
	struct terrain_palette *palette = new struct terrain_palette;
	assert(palette);
	memset(palette, 0, sizeof(struct terrain_palette));

        list_init(&palette->set);
        palette->tag = strdup(tag);
        palette->current_terrain_index = PAL_TERRAIN_NOT_SET;
        palette->num_entries = 0;

	return palette;
}

struct terrain_palette_entry *
terrain_palette_entry_new(char *glyph, struct terrain *terrain)
{
        struct terrain_palette_entry *entry;
        entry = (struct terrain_palette_entry *)malloc(sizeof(*entry));
        list_init(&entry->list);
        entry->glyph = strdup(glyph);
        assert(entry->glyph);
        entry->terrain = terrain;
        return entry;
}

void terrain_palette_entry_del(struct terrain_palette_entry *entry)
{
        // For each entry free the glyph (because we strdup'd our own copy) but
        // leave the terrain alone (it's a singleton and belongs to someone
        // else).
        free(entry->glyph);
        free(entry);
}

void terrain_palette_del(struct terrain_palette *pal)
{
        struct list *elem;

        elem = pal->set.next;
        while (elem != &pal->set) {
                struct terrain_palette_entry *entry;
                entry = outcast(elem, struct terrain_palette_entry, list);
                elem = elem->next;
                terrain_palette_entry_del(entry);
        }

        if (pal->tag)
                free(pal->tag);
        delete pal;
}

void terrain_palette_add(struct terrain_palette *pal, char *glyph, struct terrain *ter)
{
        struct terrain_palette_entry *entry;

        entry = terrain_palette_entry_new(glyph, ter);
        list_add_tail(&pal->set, &entry->list);
        pal->num_entries++;
}

struct terrain_palette_entry *palette_entry(struct terrain_palette *palette, int n)
{
        struct list *elem;

        assert(palette);
        if (palette->num_entries < 1) {
                dbg("palette_terrain_for_glyph() num_entries == 0\n");
                return 0;
        }
        if (n < 0 || n >= palette->num_entries) {
                dbg("palette_terrain_for_glyph() called with out-of-bounds "\
                    "arg n=%d\n", n);
                return 0;
        }

        elem = palette->set.next;
        while (n) {
                elem = elem->next;
                n--;
        }

        return outcast(elem, struct terrain_palette_entry, list);
}

char *palette_glyph(struct terrain_palette *palette, int n)
{
        struct terrain_palette_entry *entry;

        entry = palette_entry(palette, n);
        if (entry)
                return entry->glyph;
        return 0;
}

struct terrain_palette_entry *
palette_entry_for_terrain(struct terrain_palette * pp, struct terrain * tt)
{
        struct list *elem;
        struct terrain_palette_entry *entry;

        list_for_each(&pp->set, elem) {
                entry = outcast(elem, struct terrain_palette_entry, list);
                if (tt == entry->terrain)
                        return entry;
        }

        return 0;  // Did not find the terrain
}

char * palette_glyph_for_terrain (struct terrain_palette * pp, struct terrain * tt)
{
        struct terrain_palette_entry *entry;

        entry = palette_entry_for_terrain(pp, tt);
        if (entry)
                return entry->glyph;
        return 0;  // Did not find the terrain
}

struct terrain *palette_terrain(struct terrain_palette *palette, int n)
{
        struct terrain_palette_entry *entry;

        entry = palette_entry(palette, n);
        if (entry)
                return entry->terrain;
        return 0;
}

struct terrain *palette_terrain_for_glyph(struct terrain_palette *palette,
					  char *glyph)
{
        struct list *elem;
        struct terrain_palette_entry *entry;

        list_for_each(&palette->set, elem) {
                entry = outcast(elem, struct terrain_palette_entry, list);
                if (! strcmp(glyph, entry->glyph))
                        return entry->terrain;
        }

        return 0;  // Did not find the terrain
}				// palette_terrain_for_glyph()

struct terrain_palette * palette_contains_terrain (struct terrain_palette *pp, 
                                                   struct terrain *tt)
{
        // The current user of this function is 
        // combat.c create_camping_map().
        // It is used to find a palette (any palette)
        // which contains a certain fill terrain.
        // 
        // For other uses, I wonder if returning the index 
        // where it is found, or -1 for not found, 
        // would be more useful?
        struct terrain_palette_entry *entry;

        entry = palette_entry_for_terrain(pp, tt);
        if (entry)
                return pp;
        return 0;  // Did not find the terrain
}

struct terrain * palette_current_terrain(struct terrain_palette * pp)
{
        // Return the terrain object which is the "current" 
        // terrain for this palette.
        // If the notion of "current" terrain was not already set,
        // it is transparently set to a sensible default.
        // 
        // This function will always return a terrain when called properly;
        // this requires that the palette pp was created with 
        // new_terrain_palette() and at least one palette_entry added,
        // such as by LTP_wrapper() + load_terrain_palette_entry().
        assert(pp);
        if (pp->num_entries < 1) {
                printf("palette_current_terrain() called on an empty palette.\n");
                assert(0);  // API usage error to call on an empty palette
        }

        int index = pp->current_terrain_index;
        if (index == PAL_TERRAIN_NOT_SET) {
                // Set and return a useful value:
                // Since num_entries >= 1, we know that
                // the entry in slot zero is valid.
                pp->current_terrain_index = 0;
                return palette_terrain(pp, 0);
        }
        else if (index < 0 || index >= pp->num_entries) {
                printf("palette_current_terrain() "
                       "called with out-of-bounds arg n=%d\n", index);
                assert(0);
        }

        if (!palette_terrain(pp, index)) {
                // Somehow an entry in set[] is invalid!
                printf("palette_current_terrain() "
                       "palette entry %d is not valid!\n", index);
                assert(0);
        }
        return palette_terrain(pp, index);
}

int palette_get_current_terrain_index(struct terrain_palette * pp)
{
        assert(pp);
        return pp->current_terrain_index;
}

int palette_set_current_terrain(struct terrain_palette * pp, int n)
{
        assert(pp);
        if (n < 0 || n >= pp->num_entries) {
                printf("palette_set_current_terrain() called with out-of-bounds arg n=%d", n);
                return PAL_TERRAIN_NOT_SET;  // Invalid index
        }
        if (!palette_terrain(pp, n)) {
                // Is this case possible?
                printf("palette_set_current_terrain() called for empty slot n=%d", n);
                return PAL_TERRAIN_NOT_SET;  // Invalid index

        }
        pp->current_terrain_index = n;
        return n;  // Valid index
}

int palette_prev_terrain(struct terrain_palette * pp)
{
        assert(pp);
        int n = pp->current_terrain_index;
        n--;
        if (n < 0)
                n = (pp->num_entries - 1);
        return palette_set_current_terrain(pp, n);
}

int palette_next_terrain(struct terrain_palette * pp)
{
        assert(pp);
        int n = pp->current_terrain_index;
        n++;
        if (n >= pp->num_entries)
                n = 0;
        return palette_set_current_terrain(pp, n);
}

int palette_first_terrain(struct terrain_palette * pp)
{
        assert(pp);
        return palette_set_current_terrain(pp, 0);
}

int palette_last_terrain(struct terrain_palette * pp)
{
        assert(pp);
        int n = (pp->num_entries - 1);
        return palette_set_current_terrain(pp, n);
}

int palette_get_quick_terrain_index(struct terrain_palette * pp, int qt)
{
        assert(pp);
        if (qt < 0 || qt >= NUM_QUICK_TERRAINS) {
                printf("palette_get_quick_terrain_index() "
                       "called with out-of-bounds arg qt=%d\n", qt);
                return PAL_TERRAIN_NOT_SET;  // Invalid index
        }
        return pp->quick_terrain[qt];
}

int palette_set_quick_terrain(struct terrain_palette * pp, int qt, int index)
{
        assert(pp);
        if (qt < 0 || qt >= NUM_QUICK_TERRAINS) {
                printf("palette_quick_terrain() "
                       "called with out-of-bounds arg qt=%d\n", qt);
                return PAL_TERRAIN_NOT_SET;  // Invalid index
        }
        if (index < 0 || index >= pp->num_entries) {
                printf("palette_quick_terrain() "
                       "called with out-of-bounds arg index=%d\n", index);
                return PAL_TERRAIN_NOT_SET;  // Invalid index
        }
        pp->quick_terrain[qt] = index;
        return index;  // Valid index
}

struct terrain * palette_quick_terrain(struct terrain_palette *pp, int qt)
{
        assert(pp);
        if (qt < 0 || qt >= NUM_QUICK_TERRAINS) {
                printf("palette_quick_terrain() "
                       "called with out-of-bounds arg qt=%d\n", qt);
                assert(0);
        }
        int index = palette_get_quick_terrain_index(pp, qt);
        return palette_terrain(pp, index);
}

void palette_print(FILE * fp, int indent, struct terrain_palette *palette)
{
	int i;
        struct list *elem;
	assert(fp);

	INDENT;
	fprintf(fp, "PALETTE %s {\n", palette->tag);
	indent += INDENTATION_FACTOR;

	INDENT;
	fprintf(fp, "// num_entries  %d;\n", palette->num_entries);
        INDENT;
        fprintf(fp, "// widest_glyph %d;\n", palette->widest_glyph);

        list_for_each(&palette->set, elem) {
                struct terrain_palette_entry *entry;
                entry = outcast(elem, struct terrain_palette_entry, list);
		palette_entry_print(fp, indent, entry);
	}

	INDENT;
	fprintf(fp, "// quick terrains:\n");
	for (i = 0; i < NUM_QUICK_TERRAINS; i++) {
		struct terrain *qt = palette_quick_terrain(palette, i);
		char *tag  = "(none)";
		char *name = "(none)";
		if (qt) {
			name = qt->name;
		}
		INDENT;
		fprintf(fp, "// %d '%s' '%s'\n", i, tag, name);
	}

        indent -= INDENTATION_FACTOR;
        INDENT;
        fprintf(fp, "} // PALETTE %s\n", palette->tag);
	fprintf(fp, "\n");
} // palette_print()
