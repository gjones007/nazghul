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

#ifndef terrain_map_h
#define terrain_map_h

/**
 * A terrain map.
 */
struct terrain_map {
	char *tag;		/* for saving */
	int w;			/* width in tiles */
	int h;			/* height in tiles */
	struct terrain_palette *palette;	/* palette used to save/load */
	struct terrain **terrain;	/* terrain grid */

	void *handle;		/* pointer to session handle */
	int saved;		/* 1 iff already saved */
	int refcount;

	/* added to support composite maps */
	int submap_w;		/* submap width */
	int submap_h;		/* submap height */
	char composite:1;	/* save as composite */
};

/**
 * Constructors and ref-counting.
 */
extern "C" struct terrain_map *terrain_map_new(const char *tag,
					       unsigned int w,
					       unsigned int h,
					       struct terrain_palette *pal);
extern "C" void terrain_map_ref(struct terrain_map *map);
extern "C" void terrain_map_unref(struct terrain_map *map);
extern "C" struct terrain_map *terrain_map_clone(struct terrain_map *orig,
						 const char *tag);

/**
 * Accessors.
 */
extern "C" struct terrain *terrain_map_get_terrain(struct terrain_map *map,
						   int x, int y);

/**
 * Mutators.
 */
extern "C" void terrain_map_rotate(struct terrain_map *map, int degree);
extern "C" void terrain_map_blit(struct terrain_map *dest, int dest_x,
				 int dest_y, struct terrain_map *src,
				 int src_x, int src_y, int w, int h);
extern "C" void terrain_map_fill(struct terrain_map *map, int x, int y, int w,
				 int h, struct terrain *fill);
extern "C" void terrain_map_set_terrain(struct terrain_map *map, int x,
					int y, struct terrain *val);

/**
 * Saver.
 */
extern "C" void terrain_map_save(struct save *, void *val);

#endif
