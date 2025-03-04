/*
 nazghul - an old-school RPG engine
 Copyright (C) 2002, 2003, 2014 Gordon McNutt

 This program is free software; you can redistribute it and/or modify it
 under the terms of the GNU General Public License as published by the Free
 Software Foundation; either version 2 of the License, or (at your option)
 any later version.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 more details.

 You should have received a copy of the GNU General Public License along with
 this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
 Suite 330, Boston, MA 02111-1307 USA

 Gordon McNutt
 gmcnutt@users.sourceforge.net
*/

#include "common.h"
#include "map.h"
#include "mem.h"
#include "session.h"
#include "terrain.h"
#include "terrain_map.h"
#include <assert.h>
#include <stdio.h>


static void _terrain_map_fin(void *vptr)
{
        struct terrain_map *map = (struct terrain_map *) vptr;
        if (map->tag) {
                free(map->tag);
        }
        if (map->terrain) {
                free(map->terrain);
        }
}


struct terrain_map *terrain_map_new(const char *tag, unsigned int w,
                                    unsigned int h, struct terrain_palette *pal)
{
        struct terrain_map *map;

        map = MEM_ALLOC_TYPE(struct terrain_map, _terrain_map_fin);
        if (tag) {
                map->tag = strdup(tag);
        }
        map->w = w;
        map->h = h;
        map->palette = pal;
        map->terrain =
            (struct terrain **) calloc(sizeof (struct terrain *), w * h);
        return map;
}


void terrain_map_ref(struct terrain_map *map)
{
        mem_ref(map);
}

void terrain_map_unref(struct terrain_map *map)
{
        mem_deref(map);
}

struct terrain *terrain_map_get_terrain(struct terrain_map *map, int x, int y)
{
        return map->terrain[y * map->w + x];
}


void terrain_map_set_terrain(struct terrain_map *map, int x,
                             int y, struct terrain *val)
{
        map->terrain[y * map->w + x] = val;

}


struct terrain_map *terrain_map_clone(struct terrain_map *orig, const char *tag)
{
        struct terrain_map *map;

        if (!orig->palette) {
                err("terrain_map_clone() \n"
                    " called to clone a map (tag '%s') without a palette,\n"
                    " this may be or cause a problem elsewhere.\n", orig->tag);
        }

        map = terrain_map_new(tag, orig->w, orig->h, orig->palette);
        memcpy(map->terrain, orig->terrain,
               sizeof (struct terrain *) * orig->w * orig->h);

        map->composite = orig->composite;
        map->submap_w = orig->submap_w;
        map->submap_h = orig->submap_h;

        return map;
}


void terrain_map_rotate(struct terrain_map *map, int degree)
{
        // SAM:
        // One thing we will want to provide for in the future
        // is maps with terrain with different orientations.
        // The canonical example is diagonal wall sections
        // for NW,NE,SW,SE corners.
        // 
        // A map containing such pieces looks strange after rotation.
        // Presumably in future some means will exist of specifying 
        // that a terrain has an "orientation" or "facing" which must 
        // be preserved (by substitution of terrain and/or sprite) 
        // when the map is rotated.
        struct terrain **rbuf;
        int x1, y1, x2, y2;
        int w2 = map->w;
        int h2 = map->h;

        // Originally I tried a rotation matrix with a naive implementation,
        // but I overlooked the problem that tile coordinates do not match up
        // naturally with the point-based coordinates of the numeric axis
        // assumed by such an alg. Yes, you twiddle things to work but I don't
        // think it's worth it given the limited nature of the rotations I
        // support here. So I fell back on a set of straightforward copy
        // routines.

        rbuf =
            (struct terrain **) malloc(sizeof (struct terrain *) * map->w *
                                       map->h);
        if (rbuf == NULL) {
                err("malloc failed");
                return;
        }
        // First convert the degrees to one of the four cases.
        degree = degree % 360;
        degree = degree / 90;

        switch (degree) {
        case 0:
                // Nothing to do.
                return;
        case 1:
                // 90 degree clockwise rotation:
                // 
                // 0  1  2    9  6  3  0
                // 3  4  5 => 10 7  4  1
                // 6  7  8    11 8  5  2
                // 9 10 11
                w2 = map->h;
                h2 = map->w;
                for (y1 = 0, x2 = w2 - 1; y1 < map->h; y1++, x2--) {
                        for (x1 = 0, y2 = 0; x1 < map->w; x1++, y2++) {
                                rbuf[y2 * w2 + x2] =
                                    map->terrain[y1 * map->w + x1];
                        }
                }
                break;
        case 2:
                // 180 degree rotation:
                // 
                // 0  1  2    11 10  9 
                // 3  4  5 =>  8  7  6
                // 6  7  8     5  4  3
                // 9 10 11     2  1  0
                w2 = map->w;
                h2 = map->h;
                for (y1 = 0, y2 = h2 - 1; y1 < map->h; y1++, y2--) {
                        for (x1 = 0, x2 = w2 - 1; x1 < map->w; x1++, x2--) {
                                rbuf[y2 * w2 + x2] =
                                    map->terrain[y1 * map->w + x1];
                        }
                }
                break;
        case 3:
                // 90 degree counter-clockwise rotation:
                // 
                // 0  1  2    2  5  8 11
                // 3  4  5 => 1  4  7 10
                // 6  7  8    0  3  6  9
                // 9 10 11
                w2 = map->h;
                h2 = map->w;
                for (y1 = 0, x2 = 0; y1 < map->h; y1++, x2++) {
                        for (x1 = 0, y2 = h2 - 1; x1 < map->w; x1++, y2--) {
                                rbuf[y2 * w2 + x2] =
                                    map->terrain[y1 * map->w + x1];
                        }
                }
                break;
        default:
                assert(false);
        }

        // debug

        // Free the original copy.
        free(map->terrain);

        // Replace the original with the rotated copy.
        map->terrain = rbuf;
        map->w = w2;
        map->h = h2;

}


void terrain_map_blit(struct terrain_map *dest, int dest_x, int dest_y,
                      struct terrain_map *src, int src_x, int src_y,
                      int w, int h)
{
        int x, y;
        struct terrain **dptr, **sptr;

        // truncate dimensions if nec
        w = min(dest->w - dest_x, min(w, min(dest->w, src->w)));
        h = min(dest->h - dest_y, min(h, min(dest->h, src->h)));

        for (y = 0; y < h; y++) {
                dptr = dest->terrain + ((y + dest_y) * dest->w + dest_x);
                sptr = src->terrain + ((y + src_y) * src->w + src_x);
                for (x = 0; x < w; x++) {
                        *dptr++ = *sptr++;
                }
        }
}


void terrain_map_fill(struct terrain_map *map, int x, int y, int w, int h,
                      struct terrain *fill)
{
        int x2, y2;
        assert(map);
        assert(fill);

        for (y2 = y; y2 < (y + h); y2++) {
                if (y2 < 0)
                        continue;
                if (y2 >= map->h)
                        break;
                for (x2 = x; x2 < (x + w); x2++) {
                        if (x2 < 0)
                                continue;
                        if (x2 >= map->w)
                                break;
                        map->terrain[y2 * map->w + x2] = fill;
                }
        }
}


static void terrain_map_composite_print_block_header(struct save *save, int nn,
                                                     int n_blocks, int x, int y,
                                                     int sub_w, int sub_h)
{
        int west_x, east_x;
        int north_y, south_y;

        west_x = x * sub_w;
        east_x = ((x + 1) * sub_w) - 1;

        north_y = y * sub_h;
        south_y = ((y + 1) * sub_h) - 1;

        save->write(save, ";; Map Block #%d/%d (%d,%d)\n", nn, n_blocks, x, y);
        save->write(save, ";;	  NW=%3d,%-3d  NE=%3d,%-3d \n", west_x, north_y,        /* NW corner */
                    east_x, north_y /* NE corner */ );
        save->write(save, ";;	  SW=%3d,%-3d  SE=%3d,%-3d \n", west_x, south_y,        /* SW corner */
                    east_x, south_y /* SE corner */ );
        save->write(save, "\n");
}


static void terrain_map_composite_save(struct save *save,
                                       struct terrain_map *map)
{
        int w, h, sub_w, sub_h;
        int x, y, sub_x, sub_y;
        int nn;
        const char *tag;

        map->saved = save->session_id;

        sub_w = map->submap_w;
        sub_h = map->submap_h;

        w = map->w / sub_w;
        h = map->h / sub_h;

        /* write the composite map constructor */
        if (map->tag)
                tag = map->tag;
        else
                tag = "nil";

        save->enter(save,
                    "(kern-mk-composite-map %s%s %d %d\n",
                    map->tag ? "'" : "", tag, w, h);
        save->write(save,
                    ";; %d x %d map blocks: %d blocks, each %d x %d tiles\n",
                    w, h, (w * h), sub_w, sub_h);

        save->write(save, ";; Map block layout:\n");
        nn = 1;
        for (y = 0; y < h; y++) {
                save->write(save, ";; ");
                for (x = 0; x < w; x++) {
                        fprintf(save->file, "Block %2d (%d,%d)  ", nn, x, y);
                        nn++;
                }
                fprintf(save->file, "\n");
        }
        fprintf(save->file, "\n");

        /* for each submap */
        nn = 1;
        for (y = 0; y < h; y++) {
                for (x = 0; x < w; x++) {
                        /* write the submap constructor */
                        save->enter(save,
                                    "(kern-mk-map nil %d %d %s\n",
                                    sub_w, sub_h, map->palette->tag);
                        terrain_map_composite_print_block_header(save, nn,
                                                                 (w * h), x, y,
                                                                 sub_w, sub_h);

                        /* write the submap terrain list */
                        save->enter(save, "(list\n");

                        for (sub_y = y * map->submap_h;
                             sub_y < y * map->submap_h + map->submap_h;
                             sub_y++) {

                                save->write(save, "\"");

                                for (sub_x = x * map->submap_w;
                                     sub_x < x * map->submap_w + map->submap_w;
                                     sub_x++) {
                                        char *glyph;
                                        struct terrain *terrain =
                                            map->terrain[sub_y * map->w +
                                                         sub_x];
                                        glyph =
                                            palette_glyph_for_terrain
                                            (map->palette, terrain);
                                        if (!glyph) {
                                                err("map %s: no glyph in palette %s " "for terrain %s (%s) at [%d %d]\n", map->tag, map->palette->tag, terrain->name, terrain->tag, sub_x, sub_y);
                                                assert(glyph);
                                        }
                                        // print with no indentation (same line)
                                        fprintf(save->file, "%2s ", glyph);
                                }
                                fprintf(save->file, "\"\n");
                        }

                        save->exit(save, ")\n");
                        save->exit(save, ")\n");
                        nn++;
                }
        }

        save->exit(save, ")\n");
}


void terrain_map_save(struct save *save, void *val)
{
        struct terrain_map *map;
        int x, y, i;

        map = (struct terrain_map *) val;

        if (map->saved == save->session_id) {
                save->write(save, "%s\n", map->tag);
                return;
        }

        if (map->composite) {
                terrain_map_composite_save(save, map);
                return;
        }

        save->enter(save, "(kern-mk-map\n");
        if (map->tag)
                save->write(save, "'%s ", map->tag);
        else
                save->write(save, "nil ");
        save->write(save, "%d %d %s\n", map->w, map->h, map->palette->tag);
        save->enter(save, "(list\n");

        i = 0;
        for (y = 0; y < map->h; y++) {
                save->write(save, "\"");

                for (x = 0; x < map->w; x++) {
                        char *glyph;
                        struct terrain *terrain = map->terrain[i];
                        glyph =
                            palette_glyph_for_terrain(map->palette, terrain);
                        if (!glyph) {
                                err("map %s: no glyph in palette %s "
                                    "for terrain %s (%s) at [%d %d]\n",
                                    map->tag,
                                    map->palette->tag,
                                    terrain->name, terrain->tag, x, y);
                                assert(glyph);
                        }
                        // print with no indentation (same line)
                        fprintf(save->file, "%2s ", glyph);

                        i++;
                }
                fprintf(save->file, "\"\n");
        }
        save->exit(save, ")\n");
        save->exit(save, ")\n");

        map->saved = save->session_id;
}
