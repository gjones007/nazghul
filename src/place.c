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
/* 12/14/2002 Sam Glasby added place_get_terrain()
 */
#include "place.h"
#include "sprite.h"
#include "util.h"
#include "terrain.h"
#include "portal.h"
#include "hash.h"
#include "screen.h"
#include "player.h"
#include "sky.h"
#include "moongate.h"
#include "console.h"
#include "wq.h"
#include "Field.h"
#include "Mech.h"
#include "vehicle.h"

// #define DEBUG
// #undef debug_h
#include "debug.h"

#include <stdlib.h>
#include <string.h>
#include <math.h>

#define HIGHLIGHT_W 2
#define HIGHLIGHT_H 2

#define WRAP(c,max) (((c) + (max)) % (max))
#define WRAP_DISTANCE(a,b,max) (min((a) + (max) - (b), (b) - (a)))
#define INDEX(x,y,w) ((x) + (y) * (w))
#define WRAP_COORDS(place, x, y) do { \
        if ((place)->wraps) { \
                (x) = place_wrap_x((place), (x)); \
                (y) = place_wrap_y((place), (y)); \
        } \
} while(0)

#define TERRAIN(p,x,y) ((p)->terrain_map->terrain[(y) * \
   (p)->terrain_map->w + (x)])

struct place_pathfind_context {
	struct place *place;
	int target_x;
	int target_y;
	unsigned char pmask;
	int pflags;
};

struct place *Place;

/* Tile API ******************************************************************/
struct tile {
	struct olist hashlink;
	struct olist objstack;
	class Vehicle *vehicle;
	class Moongate *moongate;
	int objects;
	int lock;
};
static struct tile *tile_create(int hashkey)
{
	struct tile *tile;
	CREATE(tile, struct tile, 0);
	olist_init(&tile->hashlink);
	olist_init(&tile->objstack);
	tile->hashlink.key = hashkey;
	return tile;
}
static void tile_destroy(struct tile *tile)
{
	// Locking prevents me from destroying a tile while the place_for_each
	// algorithm is using it.
	if (!tile->lock) {
		assert(!tile->objects);
		list_remove(&tile->hashlink.list);
		// printf("Destroying tile %p\n", tile);
		free(tile);
	}
}

static int tile_is_transparent(struct tile *tile)
{
	struct list *list, *elem, *tmp;
	class Object *obj;

	list = &tile->objstack.list;
	elem = list->next;

	while (elem != list) {
		tmp = elem->next;
		obj = outcast(elem, class Object, container_link.list);
		elem = tmp;
		if (obj->is_opaque())
			return 0;
	}
	return 1;
}

/*****************************************************************************/

struct place *place_create(enum place_type type,
			   struct place *parent,
			   unsigned int x,
			   unsigned int y,
			   char *tag,
			   char *name,
			   int wraps, struct terrain_map *terrain_map)
{
	struct place *place;

	CREATE(place, struct place, 0);

	if (!(place->tag = strdup(tag)))
		goto fail;

	if (!(place->name = strdup(name)))
		goto fail;

	if (!(place->objects = hash_create(31)))
		goto fail;

	if (!(place->terrain_map = terrain_map_clone(terrain_map)))
		goto fail;

	place->type = type;
	place->location.place = parent;
	place->location.x = x;
	place->location.y = y;
	place->original_terrain_map = terrain_map;
	place->wraps = wraps;
	place->scale = type == wilderness_place ? WILDERNESS_SCALE : 1;

	list_init(&place->vehicles);

	return place;
      fail:
	place_destroy(place);
	return 0;
}

void place_destroy_tiles(struct place *place)
{
	int i;
	for (i = 0; i < place->objects->n; i++) {
		struct list *head;
		struct list *list;
		head = &place->objects->buckets[i].list;
		list = head->next;
		while (list != head) {
			struct list *tmp;
			struct tile *tile;
			tmp = list->next;
			tile = outcast(list, struct tile, hashlink.list);
			tile_destroy(tile);
			list = tmp;
		}
	}
}

void place_destroy(struct place *place)
{
	if (place->tag)
		free(place->tag);
	if (place->name)
		free(place->name);
	if (place->objects) {
		place_destroy_tiles(place);
		hash_destroy(place->objects);
	}
	if (place->terrain_map) {
		terrain_map_destroy(place->terrain_map);
	}
	free(place);
}

int place_is_passable(struct place *place, int x, int y,
		      unsigned char pmask, int flags)
{
	struct terrain *terrain;
	class Field *field;
	class Mech *mech;
	bool impassable_terrain;
	bool no_convenient_vehicle;

	// For a wrapping place, wrap out-of-bounds x,y
	// For a non-wrapping place, return impassable.
	if (place->wraps) {
		x = WRAP(x, place->terrain_map->w);
		y = WRAP(y, place->terrain_map->h);
	} else if (y < 0 || y >= place->terrain_map->h ||
		   x < 0 || x >= place->terrain_map->w)
		return 0;

	// Unless the destination terrain is passable, 
	// or a vehicle is present at the destination, return impassable.
	// 
	// We allow movement onto impassable terrain with a vehicle,
	// so that the vehicle can be boarded.
	// 
	// For example, a ship on water, which is impassable to a walking
	// pmask.
	terrain = place->terrain_map->terrain[y * place->terrain_map->w + x];
	impassable_terrain = (terrain->pmask & pmask) == 0;
	no_convenient_vehicle = ((flags & PFLAG_IGNOREVEHICLES) ||
				 !place_get_vehicle(place, x, y));
	if (impassable_terrain && no_convenient_vehicle)
		return 0;

	// Test for an impassable Field
	field = (class Field *) place_get_object(place, x, y, field_layer);
	if (field != NULL && (field->getObjectType()->getPmask() & pmask) == 0)
		return 0;

	// Test for an impassable Mech
	if ((flags & PFLAG_IGNOREMECHS) == 0) {
		mech = (class Mech *) place_get_object(place, x, y, 
                                                       mech_layer);
                if (mech) {
                        // dbg
                        printf("*** 0x%x 0x%x\n", pmask, mech->getPmask());
                }
		if (mech != NULL && (mech->getPmask() & pmask) == 0)
			return 0;
	}

	return 1;
}

int place_is_occupied(struct place *place, int x, int y)
{
        WRAP_COORDS(place, x, y);
	return (place_get_object(place, x, y, being_layer) != 0);
}

static struct tile *place_lookup_tile(struct place *place, int x, int y)
{
	struct olist *olist;
	if (place_off_map(place, x, y))
		return 0;
	olist = hash_lookup(place->objects, INDEX(x, y, place_w(place)));
	if (!olist)
		return 0;
	return outcast(olist, struct tile, hashlink);
}

static struct tile *place_create_and_add_tile(struct place *place, int x, int y)
{
	struct tile *tile;
	tile = tile_create(INDEX(x, y, place_w(place)));
	if (!tile)
		return 0;
	hash_add(place->objects, &tile->hashlink);
	return tile;
}

struct tile *placeGetTile(struct place *place, int x, int y)
{
	struct tile *tile = place_lookup_tile(place, x, y);
	if (tile)
		return tile;
	return place_create_and_add_tile(place, x, y);
}

static void myPaintHighlight(Object * obj, int sx, int sy)
{
	SDL_Rect rect;
	rect.x = sx - HIGHLIGHT_W;
	rect.y = sy - HIGHLIGHT_H;
	rect.w = TILE_W + 2 * HIGHLIGHT_W;
	rect.h = TILE_H + 2 * HIGHLIGHT_H;
	screenFill(&rect, White);
}

void place_paint_objects(struct place *place, int mx, int my,
                         int sx, int sy)
{
	struct list *l;
	Object *obj;
	struct tile *tile;
	struct sprite *sprite;

	tile = place_lookup_tile(place, mx, my);
	if (!tile)
		return;

	/* Check for a vehicle */
	if (tile->vehicle) {
		tile->vehicle->paint(sx, sy);
	}

	/* Check for a moongate (paint moongates before other objects so I can
	 * see the player step over it) */
	if (tile->moongate)
		tile->moongate->paint(sx, sy);

	list_for_each(&tile->objstack.list, l) {
		obj = outcast(l, Object, container_link.list);
		sprite = obj->getSprite();
		if (!sprite)
			continue;

		// hack: handle invisible objects and alpha-blending when
		// invisible objects are revealed. If the global "reveal" flag
		// is not set then do not paint invisible objects. Otherwise
		// temporarily set the object's sprite to 'shaded' and paint
		// it. After painting I need to clear the shaded flags in case
		// other objects use this sprite, too.

		if (!obj->isVisible()) {
			if (!Reveal && !obj->isShaded())
				continue;
			sprite_fade(sprite);
		}

		if (obj->isSelected())
			myPaintHighlight(obj, sx, sy);
		obj->paint(sx, sy);

		if (sprite->faded)
			sprite_unfade(sprite);
	}

}

int place_visibility(struct place *place, int x, int y)
{
	struct terrain *terrain;
	struct tile *tile;

	if (place->wraps) {
		x = WRAP(x, place->terrain_map->w);
		y = WRAP(y, place->terrain_map->h);
	} else if (x < 0 || x >= place->terrain_map->w || y < 0 ||
		   y >= place->terrain_map->h)
		return 0;

	terrain = place->terrain_map->terrain[y * place->terrain_map->w + x];
	if (!terrain || !terrain->alpha)
		return 0;

	tile = place_lookup_tile(place, x, y);
	if (tile)
		return tile_is_transparent(tile);

	return 1;
}

unsigned int place_walking_distance(struct place *place,
				    int x0, int y0, int x1, int y1)
{
	int dx;
	int dy;

	if (place->wraps) {
		dx = WRAP_DISTANCE(min(x0, x1), max(x0, x1),
				   place->terrain_map->w);
		dy = WRAP_DISTANCE(min(y0, y1), max(y0, y1),
				   place->terrain_map->h);
	} else {
		dx = max(x0, x1) - min(x0, x1);
		dy = max(y0, y1) - min(y0, y1);
	}

	return dx + dy;
}

unsigned int place_flying_distance(struct place *place,
				   int x0, int y0, int x1, int y1)
{
	int dx;
	int dy;

        place_get_direction_vector(place, x0, y0, x1, y1, &dx, &dy);
        dx = abs(dx);
        dy = abs(dy);

	// This approx comes from the angband LOS source, and overestimates
	// about one tile per fifteen tiles of distance.
	return ((dy > dx) ? (dy + (dx >> 1)) : (dx + (dy >> 1)));
}

void place_get_direction_vector(struct place *place, int x0, int y0, int x1, 
                               int y1, int *dx, int *dy)
{
        int east, west, north, south;

        // fixme: is this code assuming that (x0,y0) and (x1,y1) are already
        // wrapped?

        if (! place->wraps) {
                *dx = x1 - x0;
                *dy = y1 - y0;
                return;
        }

        // Four possibilities for dx:
        //
        // |    x0-->x1    | (a) direct east
        // |<--x0     x1<--| (b) west across map boundary
        // |    x1<--x0    | (c) direct west
        // |-->x1     x0-->| (d) east across map boundary
        //
        // Note that since west is always to the left, it is a negative vector.
        // We compute it as a positive value to make it easy to compare against
        // east, but convert it to negative before returning it as dx.

        if (x1 > x0) {
                east = x1 - x0;
                west = x0 + place->terrain_map->w - x1;
        } else {
                west = x0 - x1;
                east = x1 + place->terrain_map->w - x0;
        }

        if (west < east)
                *dx = -west;
        else
                *dx = east;

        // Four possibilities for dy:
        //
        // ---------------
        //      ^       |
        // y0   |   y1  v
        // |    y0  ^   y1
        // v        |
        // y1       y0
        //      y1      y0
        //      ^       |
        //      |       V
        // ---------------
        // (a) (b) (c) (d)
        //
        // (a) direct south
        // (b) north across map boundary
        // (c) direct north
        // (d) south across map boundary
        //
        // Note that north is always a negative vector.

        if (y1 > y0) {
                south = y1 - y0;
                north = y0 + place->terrain_map->h - y1;
        } else {
                north = y0 - y1;
                south = y1 + place->terrain_map->h - y0;
        }

        if (north < south)
                *dy = -north;
        else
                *dy = south;
}

int place_add_object(struct place *place, Object * object)
{
	struct tile *tile = place_lookup_tile(place, object->getX(),
					      object->getY());

	if (!tile) {
		tile = place_create_and_add_tile(place, object->getX(),
						 object->getY());
		if (!tile)
			return -1;
	}

	if (object->isType(VEHICLE_ID)) {
		if (tile->vehicle)
			return -1;
		tile->vehicle = (class Vehicle *) object;
		tile->objects++;
		return 0;
	}

	olist_add(&tile->objstack, &object->container_link);
	tile->objects++;
	return 0;
}

void place_add_moongate(struct place *place, class Moongate * moongate)
{
	struct tile *tile =
	    placeGetTile(place, moongate->getX(), moongate->getY());
	tile->moongate = moongate;
	tile->objects++;
}

void place_remove_object(struct place *place, Object * object)
{
	struct tile *tile = place_lookup_tile(place, object->getX(),
					      object->getY());
	assert(tile);

	if (object->isType(VEHICLE_ID)) {
		tile->vehicle = 0;
	} else {
		list_remove(&object->container_link.list);
		list_init(&object->container_link.list);
	}

	tile->objects--;
	if (!tile->objects) {
		tile_destroy(tile);
	}
}

Object *place_get_object(struct place *place, int x, int y, enum layer layer)
{
	struct olist *olist;
	struct tile *tile;

        WRAP_COORDS(place, x, y);

	tile = place_lookup_tile(place, x, y);
	if (!tile)
		return 0;
	olist =
	    olist_lookup(&tile->objstack, layer, 0 /* find the top object */ );
	if (!olist)
		return 0;

	return outcast(olist, Object, container_link);
}

class Portal *place_get_portal(struct place * place, int x, int y)
{
	Object *object;

        WRAP_COORDS(place, x, y);

	object = place_get_object(place, x, y, portal_layer);
	if (!object)
		return 0;

	return (class Portal *) object;
}

class NpcParty *place_get_NpcParty(struct place * place, int x, int y)
{
	Object *object;

        WRAP_COORDS(place, x, y);

	// fixme: this will get Character's, too... and be careful because the
	// pathfinding code appears to rely on this fact.
	object = place_get_object(place, x, y, being_layer);
	if (!object)
		return 0;

	if (!object->isType(NPCPARTY_ID))
		return 0;

	return (class NpcParty *) object;
}

class Vehicle *place_get_vehicle(struct place * place, int x, int y)
{
	struct tile *tile;

        WRAP_COORDS(place, x, y);
	tile = place_lookup_tile(place, x, y);
	if (!tile)
		return 0;
	return tile->vehicle;
}

struct terrain_map *place_get_combat_terrain_map(struct place *place,
						 int x, int y)
{
	struct terrain *terrain;

        WRAP_COORDS(place, x, y);
	terrain = place->terrain_map->terrain[y * place->terrain_map->w + x];
	return terrain_combat_map(terrain);
}

/* Pathfinding ***************************************************************/

static int place_pathfind_is_valid_location(struct place_pathfind_context
					    *context, int x, int y)
{
	class Portal *portal;
	class Moongate *moongate;

	/* I used to check this after passability, but it really belongs first.
	 * In several cases the target location may not be passable but if the
	 * seeker can get adjacent to it that will be good enough. */
	if (x == context->target_x && y == context->target_y)
		return 1;

	if (!place_is_passable(context->place, x, y, context->pmask,
			       context->pflags))
		return 0;

	if (! (context->pflags & PFLAG_IGNOREBEINGS) &&
            place_is_occupied(context->place, x, y))
		return 0;

#ifdef PLAYER_PARTY_NOT_AN_OBJECT
	/* I added a check for the player so that NPCs will pathfind around the
	 * player when commuting. */
	if (player_party->context != CONTEXT_COMBAT &&
	    player_party->getX() == x && player_party->getY() == y)
		return 0;
#endif

	/* I used to penalize portals in the heuristic routine, but that was
	 * back in the day when I would pathfind for the player on a
	 * right-click. Any more pathfinding is used exclusively for NPCs and I
	 * NEVER want them to enter a portal unless they explicitly want to
	 * (and currently they never do). Likewise for open moongates. */
	if ((portal = place_get_portal(context->place, x, y)) &&
	    portal->isAutomatic())
		return 0;
	if ((moongate = place_get_moongate(context->place, x, y)) &&
	    moongate->isOpen())
		return 0;

	return 1;
}

static int place_pathfind_heuristic(struct astar_search_info *info)
{
	int cost = 0;
	struct terrain *terrain;
	struct place_pathfind_context *context;

	context = (struct place_pathfind_context *) info->context;

	/* The basic cost is walking distance. Duplicate that algorithm except
	 * pay attention to the info->flags. */

	if (info->flags & ASTAR_HORZ == 0) {
		// Yes, we are interested in the x coordinate of the
		// destination.
		if (context->place->wraps) {
			cost += WRAP_DISTANCE(min(info->x0, info->x1),
					      max(info->x0, info->x1),
					      context->place->terrain_map->w);
		} else {
			cost += max(info->x0, info->x1) - min(info->x0,
							      info->x1);
		}
	}

	if (info->flags & ASTAR_VERT == 0) {
		// Yes, we are interested in the y coordinate of the
		// destination.
		if (context->place->wraps) {
			cost += WRAP_DISTANCE(min(info->y0, info->y1),
					      max(info->y0, info->y1),
					      context->place->terrain_map->h);
		} else {
			cost += max(info->y0, info->y1) -
			    min(info->y0, info->y1);
		}
	}

	/* And I penalize tiles with portals to encourage the pathfinding
	 * algorithm to route around them. Make them cost a little bit more
	 * than walking all the way around them. */
	if (place_get_portal(context->place, info->x0, info->y0))
		cost += 9;

	/* And penalize tiles with hazards on them. I really should assign
	 * different penalties to different hazerds. */
	terrain = place_get_terrain(context->place, info->x0, info->y0);
	if (terrain->effects)
		cost += 2;

	if (place_get_object(context->place, info->x0, info->y0,
			     field_layer) != NULL)
		cost += 20;

	return cost;
}

struct astar_node *place_find_path(struct place *place,
				   struct astar_search_info *info,
				   unsigned char pmask)
{
	struct astar_node *path;
	struct place_pathfind_context context;

	/* Store the target location as the context */
	context.place = place;
	context.target_x = info->x1;
	context.target_y = info->y1;
	context.pmask = pmask;
	context.pflags = info->flags;

	/* Fill out the search information */
	info->is_valid_location =
	    (int (*)(void *, int, int)) place_pathfind_is_valid_location;
	info->heuristic = place_pathfind_heuristic;
	info->width = place_w(place);
	info->height = place_h(place);
	info->wraps = place->wraps;
	info->context = &context;

	/* Run the pathfinding alg */
	path = astar_search(info);

	return path;

}

int place_get_light(struct place *place, int x, int y)
{
	int light;
	struct list *l;
	Object *obj;
	struct tile *tile;

        if (place->wraps) {
                x = place_wrap_x(place, x);
                y = place_wrap_y(place, y);
        }

	/* Check if the coordinates are off-map */
	else if (place_off_map(place, x, y))
		return 0;

	/* Assign lighting from terrain */
	light =
	    place->terrain_map->terrain[y * place->terrain_map->w + x]->light;

	/* Assign lighting from tile objects */
	tile = place_lookup_tile(place, x, y);
	if (!tile)
		return light;

	/* Check for a vehicle */
	if (tile->vehicle)
		light += tile->vehicle->getLight();

	/* Check all objects */
	list_for_each(&tile->objstack.list, l) {
		obj = outcast(l, Object, container_link.list);
		light += obj->getLight();
	}

	/* Check for a moongate */
	if (tile->moongate)
		light += tile->moongate->getLight();

	return light;
}

void placeExit(void)
{

}

static void myResetObjectTurns(class Object * obj, void *data)
{
	obj->synchronize(Turn);
	// obj->setTurn(Turn);
}

void placeEnter(void)
{
	placeForEachObject(myResetObjectTurns, 0);
	// placeDumpObjects(); // For debugging output
}

class Moongate *place_get_moongate(struct place *place, int x, int y)
{
        WRAP_COORDS(place, x, y);
	struct tile *tile = place_lookup_tile(place, x, y);
	if (!tile)
		return 0;
	return tile->moongate;
}

void placeAddObject(Object * object)
{
	place_add_object(Place, object);
}

void placeRemoveObject(Object * object)
{
	place_remove_object(Place, object);
}

class NpcParty *placeGetNPC(int x, int y)
{
        WRAP_COORDS(Place, x, y);
	return place_get_NpcParty(Place, x, y);
}

int place_get_movement_cost(struct place *place, int x, int y)
{
        WRAP_COORDS(place, x, y);
	struct terrain *t = TERRAIN(place, x, y);
	return (place->scale * (t ? t->movement_cost : 0));
}

int place_is_hazardous(struct place *place, int x, int y)
{
        WRAP_COORDS(place, x, y);
	struct terrain *t = TERRAIN(place, x, y);
	return (t->effects != 0);
}

int place_adjust_turn_cost(struct place *place, int turns)
{
        return place->scale * turns;
}

int placeGetMovementCost(int x, int y)
{
        WRAP_COORDS(Place, x, y);
	struct terrain *t = TERRAIN(Place, x, y);
	return (t ? t->movement_cost : 0);
}

struct terrain *placeGetTerrain(int x, int y)
{
        WRAP_COORDS(Place, x, y);
	return TERRAIN(Place, x, y);
}

void place_set_terrain(struct place *place, int x, int y,
		       struct terrain *terrain)
{
        WRAP_COORDS(place, x, y);
	TERRAIN(place, x, y) = terrain;
}

struct terrain *place_get_terrain(struct place *place, int x, int y)
{
        WRAP_COORDS(place, x, y);
	if (place_off_map(place, x, y))
		return NULL;
	x = place_wrap_x(place, x);
	y = place_wrap_y(place, y);
	return TERRAIN(place, x, y);
}

Uint32 place_get_color(struct place * place, int x, int y)
{
        WRAP_COORDS(place, x, y);
	struct terrain *terrain = place_get_terrain(place, x, y);
	return (terrain == NULL ? 0 : terrain->color);
}

int placeWrapX(int x)
{
        // obsolete
	return place_wrap_x(Place, x);
}

int placeWrapY(int y)
{
        // obsolete
	return place_wrap_y(Place, y);
}

static void myPlaceDescribeTerrain(int x, int y)
{
	struct terrain *t = placeGetTerrain(x, y);
	consolePrint("%s", t->name);        
}

static int myPlaceDescribeObjects(int x, int y, int first_thing_listed)
{

	struct list *l;
	struct tile *tile;
	Object *obj = NULL, *prev_obj = NULL;
	class ObjectType *type = NULL;
	int n_instances;
        int n_types;
        int n_described = 0;

	tile = place_lookup_tile(Place, x, y);
	if (!tile)
		return n_described;
        

        // Let's make things simple. Inefficient, but simple. Efficiency is not
        // so critical here. We'll do this in two passes. Pass one will count
        // the number of things we need to list. Pass two will print the things
        // with the proper punctuation.

        // Step 1: count the number of different types of things we need to
        // list (multiple counts of one type of thing count as 1)

        type = NULL;
        n_types = 0;

	list_for_each(&tile->objstack.list, l) {

		obj = outcast(l, Object, container_link.list);

		if (obj->container_link.key == cursor_layer)
                        // Special case: don't describe the cursor
                        continue;


		if (type == NULL) {

                        // This is the first type of thing we need to list.
			type = obj->getObjectType();
                        if (obj->isVisible() || Reveal || obj->isShaded())
                                n_types++;

		} else if (obj->getObjectType() != type) {

                        // We just found a new type of thing (we know because
                        // it's different from the last type of thing).
			type = obj->getObjectType();
                        if (obj->isVisible() || Reveal || obj->isShaded())
                                n_types++;

		}
	}

        if (tile->vehicle && (tile->vehicle->isVisible() || Reveal || 
                              obj->isShaded()))
                n_types++;

        if (tile->moongate && (tile->moongate->isOpen() || Reveal || 
                               obj->isShaded()))
                n_types++;


        if (n_types == 0)
                // Nothing to list so we're done.
                return n_described;


        // Step 2: now we actually list the things, using the count to help us
        // decide how to punctuate.

        n_instances = 0;
        type = NULL;
        prev_obj = NULL;

	list_for_each(&tile->objstack.list, l) {

		obj = outcast(l, Object, container_link.list);

		if (obj->container_link.key == cursor_layer)
                        // Special case: don't describe the cursor
                        continue;

                printf("%s\n", obj->getName());

		if (prev_obj == NULL) {

                        // This is the first type of thing we need to
                        // list. Don't print it until we find out how many
                        // there are.
			type = obj->getObjectType();
                        n_instances = 1;

		} else if (obj->getObjectType() != type) {

                        // We just found a new type of thing (we know because
                        // it's different from the last type of thing). Now we
                        // can print the last type of thing since we know how
                        // many there are of it.

                        if (prev_obj->isVisible() || Reveal || 
                            prev_obj->isShaded()) {
                                if (first_thing_listed) {
                                        first_thing_listed = 0;
                                } else {
                                        if (n_types == 1)
                                                consolePrint(" and ");
                                        else
                                                consolePrint(", ");
                                }

                                prev_obj->describe(n_instances);
                                n_described++;
                                n_types--;
                        }

			type = obj->getObjectType();
                        n_instances = 1;

		} else {
                        // More of the same.
                        n_instances++;
                }

                prev_obj = obj;
	}

        // Now we have to print the last object in the stack.
        if (prev_obj && (prev_obj->isVisible()  || Reveal || 
                         prev_obj->isShaded())) {
                if (!first_thing_listed) {
                        if (n_types == 1)
                                consolePrint(" and ");
                        else
                                consolePrint(", ");
                }
                printf("### %s\n", prev_obj->getName());
                prev_obj->describe(n_instances);
                n_described++;
                n_types--;
        }

        if (tile->vehicle && (tile->vehicle->isVisible() || Reveal || 
                              obj->isShaded())) {
                if (n_types == 1)
                        consolePrint(" and ");
                else
                        consolePrint(", ");
                tile->vehicle->describe(1);
                n_described++;
                n_types--;
        }

        if (tile->moongate && (tile->moongate->isOpen() || Reveal || 
                               obj->isShaded())) {
                assert(n_types == 1);
                consolePrint(" and ");
                tile->moongate->describe(1);
                n_described++;
                n_types--;
        }

        return n_described;

}				// myPlaceDescribeObjects()

static void myDumpObject(class Object * obj, void *data)
{
	printf("\t%p %s [%d,%d]\n", obj, obj->getName(), obj->getX(),
	       obj->getY());
}

void placeDescribe(int x, int y, int flags)
{
        int count = 0;

        WRAP_COORDS(Place, x, y);

	if (place_off_map(Place, x, y)) {
		consolePrint("nothing!");
		return;
	}
        if (flags & PLACE_DESCRIBE_TERRAIN) {
                myPlaceDescribeTerrain(x, y);
                count = 1;
        }
        if (flags & PLACE_DESCRIBE_OBJECTS)
                count += myPlaceDescribeObjects(x, y, 
                                       (flags & PLACE_DESCRIBE_TERRAIN) == 0);
        if (!count)
                consolePrint("nothing!\n");
        else
                consolePrint(".\n");
}

void place_for_each_object(struct place *place, void (*fx) (class Object *,
							    void *data),
			   void *data)
{
	int i;
	struct olist *tileList, *objList;
	struct list *tileElem, *tileTmp, *objElem, *objTmp;
	struct tile *tile;
	class Object *obj;

	// for each bucket
	for (i = 0; i < place->objects->n; i++) {

		tileList = &place->objects->buckets[i];
		tileElem = tileList->list.next;
		assert(tileElem->prev == &tileList->list);

		// for each tile
		while (tileElem != &tileList->list) {

			tileTmp = tileElem->next;
			tile = outcast(tileElem, struct tile, hashlink.list);
			tile->lock++;

			objList = &tile->objstack;
			objElem = objList->list.next;

			// for each object
			while (objElem != &objList->list) {

				objTmp = objElem->next;
				obj = outcast(objElem, class Object,
					      container_link.list);
				fx(obj, data);

				objElem = objTmp;
			}

			// Unlock the tile. One possible consequence of the
			// above loop is that all of the objects were removed
			// from this tile, in which case we probably tried to
			// destroy it but were prevented by the lock.
			tile->lock--;
			if (!tile->objects)
				tile_destroy(tile);

			tileElem = tileTmp;
		}
	}
}

void placeForEachObject(void (*fx) (class Object *, void *data), void *data)
{
	place_for_each_object(Place, fx, data);
}

static void myAdvanceObjectTurns(class Object * obj, void *data)
{
	// Bugfix: check for Quit in this loop. If an npc party attacks the
	// player over a town and the player quits combat then the npc party
	// remains in town, but we want to stop processing any more turn points
	// it might have.
	while (obj->getTurn() < Turn && !Quit) {

		obj->advanceTurn(Turn);

		if (obj->isDestroyed()) {
			// placeRemoveObject(obj);
			delete obj;
			return;
		}
	}
}

void placeAdvanceTurns(void)
{
	placeForEachObject(myAdvanceObjectTurns, 0);
}

static void myAdvanceCombatTurn(class Object * obj, void *data)
{
	obj->advanceTurn(Turn);
	if (obj->isDestroyed()) {
		delete obj;
	}
}

void placeAdvanceCombatTurn(void)
{
	placeForEachObject(myAdvanceCombatTurn, 0);
}

void placeInit(void)
{
}

void placeDumpObjects(void)
{
	printf("Objects in %s:\n", Place->name);
	placeForEachObject(myDumpObject, 0);
}

class NpcParty *place_random_encounter(struct place *place)
{
	int i;
	class NpcPartyType *type;
	class NpcParty *npc;

	if (place->type != wilderness_place)
		// Random encounters only occur in the wilderness
		return 0;

	// Roll to generate an encounter.
	for (i = 0; i < place->n_typ_npc_parties; i++) {
		if ((random() % 10000) <= place->typ_npc_parties[i].prob) {

			// Create the party
			type = place->typ_npc_parties[i].type;
			npc = new NpcParty();
			if (!npc)
				return 0;
			npc->init(type);
			npc->setTurn(Turn);
			npc->setAlignment(place->typ_npc_parties[i].align);
			npc->createMembers();
			return npc;
		}
	}

	return 0;

}

void place_clip_to_map(struct place *place, int *x, int *y)
{
	if (place->wraps)
		return;

	*x = max(*x, 0);
	*x = min(*x, place_w(place) - 1);
	*y = max(*y, 0);
	*y = min(*y, place_h(place) - 1);
}

// fixme -- combine with mapAnimateProjectile(), use callback for animation
int place_los_blocked(struct place *place, int Ax, int Ay, int Bx, int By)
{
        // Should be called from source to target. Does not test for los on
        // target tile itself, but stops one short. So opaque tiles are visible
        // if they are the destination.

        // Apply the bresenhaum algorithm to walk the line from (x0, y0) to
        // (x1, y1) and check for visibility at each step. Note that the real
        // intention here is to see if I can fire an arrow from one point to
        // another. The missile flight code in Missile:animate() uses a test
        // for visibility on each tile to determine if a missile is blocked in
        // its flight path (missiles don't have a pmask...).

        int steps = 0;

        //Ax = place_wrap_x(place, Ax);
        //Ay = place_wrap_y(place, Ay);
        //Bx = place_wrap_x(place, Bx);
        //By = place_wrap_y(place, By);

        int Px = Ax;
        int Py = Ay;

        // Get the distance components
        int dX = Bx - Ax;
        int dY = By - Ay;
        int AdX = abs(dX);
        int AdY = abs(dY);

        // Moving left?
        int Xincr = (Ax > Bx) ? -1 : 1;

        // Moving down?
        int Yincr = (Ay > By) ? -1 : 1;

        // Walk the x-axis
        if (AdX >= AdY) {

                int dPr = AdY << 1;
                int dPru = dPr - (AdX << 1);
                int P = dPr - AdX;

                // For each x
                for (int i = AdX; i >= 0; i--) {

                        if (steps > 1 && i > 0) {
                                if (!place_visibility(place, Px, Py))
                                        return 1;
                        }

                        steps++;

                        if (P > 0) {
                                Px += Xincr;
                                Py += Yincr;
                                P += dPru;
                        }
                        else {
                                Px += Xincr;
                                P += dPr;
                        }
                }
        }
        // Walk the y-axis
        else {
                int dPr = AdX << 1;
                int dPru = dPr - (AdY << 1);
                int P = dPr - AdY;

                // For each y
                for (int i = AdY; i >= 0; i--) {

                        if (steps > 1 && i > 0) {
                                if (!place_visibility(place, Px, Py))
                                        return 1;
                        }

                        steps++;

                        if (P > 0) {
                                Px += Xincr;
                                Py += Yincr;
                                P += dPru;
                        }
                        else {
                                Py += Yincr;
                                P += dPr;
                        }
                }
        }

        return 0;
}


#ifdef PLACE_LOAD_CODE_REWRITTEN
// started rewriting all the code to load places, almost done but don't want to
// bother debugging it.
static struct typ_npc_party_info *load_typ_npc_parties(class Loader * loader,
						       int *n)
{
	struct typ_npc_party_info *info, tmp;
	int index;
	char *tag = 0;

	// base case
	if (loader->matchToken('}')) {
		info = new struct typ_npc_party_info[*n];
		if (info)
			memset(info, 0, *n * sizeof(struct typ_npc_party_info));
		return info;
	}
	// recursive case
	if (!loader->getWord(&tag))
		return 0;

	if (!(tmp.type = loader->lookupTag(tag, NPCPARTY_TYPE_ID))) {
		loader->setError("Invalid NPCPARTY tag '%s'", tag);
		free(tag);
		return 0;
	}
	free(tag);

	if (!loader->getInt(&tmp.prob))
		return 0;

	index = *n;
	(*n)++;

	if (!(info = load_typ_npc_parties(loader, n)))
		return 0;

	info[index] = tmp;

	return info;

}

struct place *placeLoad(class Loader * loader)
{
	struct place *place;
	char *ptag = 0, *mtag = 0, *otag = 0, *ttag = 0;
	class ObjectType *type;
	class Object *obj;
	class Character *ch;

	place = new struct place;
	if (!place)
		return 0;
	memset(place, 0, sizeof(*place));

	// *** Parse Parameters ***

	if (!loader->matchWord("type") ||
	    !loader->getInt(&place->type) ||
	    !loader->matchWord("parent") ||
	    !loader->getWord(&ptag) ||
	    !loader->matchWord("x") ||
	    !loader->getInt(&place->location.x) ||
	    !loader->matchWord("y") ||
	    !loader->getInt(&place->location.y) ||
	    !loader->getWord("name") ||
	    !loader->getString(&place->name) ||
	    !loader->matchWord("wraps") ||
	    !loader->getBool(&place->wraps) ||
	    !loader->matchWord("map") ||
	    !loader->getWord(&mtag) ||
	    !loader->matchWord("underground") ||
	    !loader->getBool(&place->underground))
		goto fail;

	// *** Bind Parameters ***

	// parent place
	if (strcmp(ptag, "null")) {
		if (!(place->parent = loader->lookupTag(ptag, PLACE_ID))) {
			loader->setError("Invalid PLACE tag '%s'", ptag);
			goto fail;
		}
	} else if (place->type != wilderness_type) {
		loader->setError("Any non-wilderness PLACE must have "
				 "a non-null parent PLACE");
		goto fail;
	}
	// terrain map
	if (!(place->terrain_map = loader->lookupTag(mtag, MAP_ID))) {
		loader->setError("Invalid MAP tag '%s'", mtag);
		goto fail;
	}
	// *** Parse and Bind Object List ***

	if (!loader->matchWord("objects") || !loader->matchToken('{'))
		goto fail;

	while (!loader->matchToken('}')) {

		// Get the type tag
		if (!loader->getWord(&otag))
			goto fail;

		// Check for basic object or NPC party types
		if ((type = loader->lookupTag(otag, OBJECT_ID)) ||
		    (type = loader->lookupTag(otag, NPCPARTY_TYPE_ID))) {
			if (!(obj = type->createInstance())) {
				loader->setError("Memory allocation failed");
				goto fail;
			}

		}
		// Check for an NPC character
		else if ((ch = loader->lookupTag(otag, CHARACTER_ID))) {
			if (!(obj = new NpcParty())) {
				loader->setError("Memory allocation failed");
				goto fail;
			}
			((class NpcParty *) obj)->init((class Character *) ch);
		} else {
			loader->setError("Invalid type tag '%s'", otag);
			goto fail;
		}

		free(otag);
		otag = 0;

		obj->setPlace(place);

		// Have the object parse its own fields
		if (!obj->load(loader)) {
			delete obj;
			goto fail;
		}

		if (place_add_object(place, obj)) {
			loader->setError("Failed to add object %s to place "
					 "%s (hint: if the object is a "
					 "vehicle then only one vehicle may "
					 "occupy a tile)", object->getName(),
					 place->name);
			delete obj;
		}
	}

	// *** Parse and Bind NPC Party List ***

	if (place->type == wilderness_place &&
	    (!loader->matchWord("typical_npc_parties") ||
	     !loader->matchToken('{') ||
	     (!(place->typ_npc_parties =
		load_typ_npc_parties(loader, &place - n_typ_npc_parties)))))
		goto fail;

	while (!loader->matchToken('}')) {
		assert(0);
	}

	// *** Bind Town Object

	if (place->type == town_place) {

		if (!loader->matchWord("object_type") ||
		    !loader->getWord(&ttag))
			goto fail;

		if (!(type = loader->lookupTag(ttag, OBJECT_ID))) {
			loader->setError goto fail;
		}

		if (!(portal = new Portal()))
			}

		      cleanup:
			if (ptag)
				free(ptag);
		if (mtag)
			free(mtag);

		return place;

	      fail:
		place_destroy(place);
		place = 0;
		goto cleanup;
	}
}
#endif				// PLACE_LOAD_CODE_REWRITTEN
