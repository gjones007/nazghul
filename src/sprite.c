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
#include "cmdwin.h"
#include "images.h"
#include "list.h"
#include "map.h"
#include "mem.h"
#include "screen.h"
#include "session.h"
#include "sprite.h"
#include "tick.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* rsurf - wraps a surface with a reference count so sprites can share them
 * without fear of premature calls to SDL_FreeSurface(). */
struct rsurf {
        int ref;                /* reference count                     */
        SDL_Surface *surf;      /* underlying surface                  */
        char custom:1;          /* NOT referenced by any struct images */
};

/* sprite - animation sequence with different facings */
struct sprite {
        char *tag;              /* Script variable name for the sprite.    */
        int n_frames;           /* per sequence                            */
        int n_total_frames;     /* n_frames x # facings                    */
        SDL_Rect *frames;       /* all frames (sequences must be in order) */
        struct rsurf *rsurf;    /* source of image                         */
        int facing;             /* current facing sequence                 */
        int facings;            /* bitmap of supported facing sequences    */
        int sequence;           /* current animation sequence              */
        struct sprite *decor;   /* decoration sprites                      */
        int w_pix, h_pix;       /* frame dimensions (in pixels)            */
        int faded:1;            /* render sprite sem-transparent           */
        int wave:1;             /* vertical roll                           */
};

typedef struct {
        struct list list;
        void (*callback) (void *arg);
        void *arg;
} sprite_watcher_t;


static struct list sprite_watchers;
static int sprite_zoom_factor = 1;
static unsigned int sprite_ticks = 0;

static struct rsurf *rsurf_new(SDL_Surface * surf)
{
        struct rsurf *rsurf;

        rsurf = (struct rsurf *) calloc(1, sizeof (*rsurf));
        if (!rsurf) {
                return 0;
        }
        rsurf->ref = 1;
        rsurf->surf = surf;
        return rsurf;
}

static void rsurf_unref(struct rsurf *rsurf)
{
        assert(rsurf->ref > 0);
        rsurf->ref--;
        if (!rsurf->ref) {
                if (rsurf->surf && rsurf->custom) {
                        SDL_FreeSurface(rsurf->surf);
                }
                free(rsurf);
        }
}

/**
 * For reasons I don't quite understand, doing an RGBA->RGBA blit with
 * SDL_BlitSurface() seems to result in a totally transparent image. I wrote
 * this as a debug measure, but since it works, and I don't expect it to run in
 * a performance-critical part of the code, I'm leaving it in for now.
 *
 * @param source The surface to blit from.
 * @param from The area of the source to blit from.
 * @param dest The surface to blit to.
 * @param to The area of the destination to blit to.
 */
static void sprite_custom_blit(SDL_Surface * source, SDL_Rect * from,
                               SDL_Surface * dest, SDL_Rect * to)
{
        Uint8 *dpix, *spix, pix = 0;
        int dx, dy, pix_bytes;
        Uint8 in_alpha;

        spix = (Uint8 *) (source->pixels);
        dpix = (Uint8 *) (dest->pixels);

        pix_bytes = source->format->BytesPerPixel;
        assert(pix_bytes == 1 || pix_bytes == 2 || pix_bytes == 4);
        assert(dest->format->BytesPerPixel == pix_bytes);

        for (dy = 0; dy < from->h; dy++) {
                for (dx = 0; dx < from->w; dx++) {

                        switch (pix_bytes) {
                        case 4:
                                pix = *(Uint32 *) spix;
                                break;
                        case 2:
                                pix = *(Uint16 *) spix;
                                break;
                        case 1:
                                pix = *(Uint8 *) spix;
                                break;
                        }
                        /* Extract the alpha component of the source pixel. */
                        in_alpha = ((pix & source->format->Amask)
                                    >> source->format->Ashift);

                        /* Skip transparent source pixels, leaving destination
                         * intact. */
                        if (SDL_ALPHA_TRANSPARENT == in_alpha) {
                                continue;
                        }

                        /* Do a direct copy of everything else. Note that this
                         * is only correct if the source alpha is opaque. We
                         * really should blend semi-transparent source
                         * pixels. */
                        switch (pix_bytes) {
                        case 4:
                                *(Uint32 *) dpix = pix;
                                break;
                        case 2:
                                *(Uint16 *) dpix = pix;
                                break;
                        case 1:
                                *(Uint8 *) dpix = pix;
                                break;
                        }
                }
        }
}

/**
 * Replace the sprite's current image surface with a reference-counted copy.
 *
 * @param sprite The sprite to modify.
 * @returns 0 on success or -1 on error. An error occurs if the surface cannot
 * be copied.
 */
static int sprite_clone_and_replace_rsurf(struct sprite *sprite)
{
        SDL_Surface *dest = 0;
        SDL_Surface *source = sprite->rsurf->surf;
        SDL_Rect to;
        int i;

        /* Create a new surface so that the original (which may be shared with
         * other sprites) is not altered. */
        dest = SDL_CreateRGBSurface(source->flags,
                                    sprite->w_pix * sprite->n_total_frames,
                                    sprite->h_pix,
                                    source->format->BitsPerPixel,
                                    source->format->Rmask,
                                    source->format->Gmask,
                                    source->format->Bmask,
                                    source->format->Amask);
        if (!dest) {
                perror_sdl("SDL_CreateRGBSurface");
                return -1;
        }

        /* Copy each frame of the sprite to the new surface. */
        to.x = 0;
        to.y = 0;
        to.w = sprite->w_pix;
        to.h = sprite->h_pix;
        for (i = 0; i < sprite->n_total_frames; i++) {
                to.x = i * sprite->w_pix;

                /* Blit the frame. */
                sprite_custom_blit(sprite->rsurf->surf,
                                   &sprite->frames[i], dest, &to);

                /* Fixup the frames as we go. */
                sprite->frames[i] = to;
        }

        /* If the original surface was a custom rsurf then unref it. */
        if (sprite->rsurf->custom) {
                rsurf_unref(sprite->rsurf);
        }

        /* Stash the surface in a new refcounted surf wrapper. */
        sprite->rsurf = rsurf_new(dest);
        sprite->rsurf->custom = 1;

        return 0;
}

static void sprite_blit_faded(SDL_Surface * source, SDL_Rect * from,
                              SDL_Rect * to)
{
        int dx, dy, di, sx, sy, si, spitch, dpitch;
        Uint32 *dpix, *spix, pixel;
        Uint8 pix_alpha;
        SDL_Surface *tmp = 0;

        tmp = SDL_CreateRGBSurface(source->flags,
                                   from->w, from->h,
                                   source->format->BitsPerPixel,
                                   source->format->Rmask,
                                   source->format->Gmask,
                                   source->format->Bmask,
                                   source->format->Amask);
        if (tmp == NULL) {
                perror_sdl("SDL_CreateRGBSurface");
                return;
        }

        dpix = (Uint32 *) tmp->pixels;
        spix = (Uint32 *) source->pixels;

        dpitch = tmp->pitch / tmp->format->BytesPerPixel;
        spitch = source->pitch / source->format->BytesPerPixel;

        for (dy = 0; dy < from->h; dy++) {
                sy = dy;
                for (dx = 0; dx < from->w; dx++) {
                        sx = dx;
                        di = (dy * dpitch + dx);
                        si = (sy + from->y) * spitch + (sx + from->x);

                        /* Cut alpha component in half. */
                        pixel = spix[si];
                        pix_alpha = ((pixel & source->format->Amask)
                                     >> source->format->Ashift);
                        pix_alpha /= 2;
                        pixel &= ~source->format->Amask;
                        pixel |= (pix_alpha << source->format->Ashift);

                        /* Assign result. */
                        dpix[di] = pixel;
                }
        }

        screen_blit(tmp, NULL, to);
        SDL_FreeSurface(tmp);
}

static void sprite_paint_wave(struct sprite *sprite, int frame, int x, int y)
{
        SDL_Rect src;
        SDL_Rect dest;
        int wavecrest;

        frame = (frame + sprite_ticks) % sprite->n_frames;

        /* Offset the index into the current sequence */
        frame += sprite->sequence * sprite->n_frames;

        // Subtle: when rendering wave sprites zoomed, we'll get artifacts due
        // to roundoff errors in integer division. Unless we align the
        // wavecrest to the zoom factor. So for example, if we zoom at a factor
        // of two then the wavecrest must be a multiple of 2. Since we only
        // support a zoom factor of 2 right now, the simplest thing to do is
        // always use 2.
        wavecrest = (sprite_ticks * 2) % sprite->h_pix;
        wavecrest = sprite->h_pix - wavecrest;  // make it roll south

        /* Wave sprites are painted in two blits. The first blit copies
         * everything below the wavecrest to the top part of the onscreen tile.
         * The second blit copies everything above the wavecrest to the
         * bottom part of the onscreen tile. This gives the appearance of a
         * wave rolling over the tile in a direction opposite the wavefront. */

        src = sprite->frames[frame];
        src.y += wavecrest;     /* fixme -- only works because source
                                 * image has one column of sprites */
        src.h -= wavecrest;

        dest.x = x;
        dest.y = y;
        dest.w = sprite->w_pix;
        dest.h = src.h;

        if (sprite->faded) {
                sprite_blit_faded(sprite->rsurf->surf, &sprite->frames[frame],
                                  &dest);
        } else {
                screen_blit(sprite->rsurf->surf, &src, &dest);
        }

        src = sprite->frames[frame];
        src.h = wavecrest;

        dest.x = x;
        dest.y = dest.y + (sprite->h_pix - wavecrest) / sprite_zoom_factor;
        dest.w = sprite->w_pix;
        dest.h = src.h;

        if (sprite->faded) {
                sprite_blit_faded(sprite->rsurf->surf, &sprite->frames[frame],
                                  &dest);
        } else {
                screen_blit(sprite->rsurf->surf, &sprite->frames[frame], &dest);
        }

}

static void sprite_paint_normal(struct sprite *sprite, int frame, int x, int y)
{
        SDL_Rect dest;

        dest.x = x;
        dest.y = y;
        dest.w = sprite->w_pix;
        dest.h = sprite->h_pix;

        /* If the sprite is larger than a tile, ASSUME (watch out!) we're
         * blitting a giant character to the map. In this case the bottom of
         * the sprite will still line up with the bottom of the tile and it
         * will be horizontally-centered, making the left, right and top
         * overlap the neighboring tiles.
         */
        if (sprite->w_pix > TILE_W) {
                dest.x -= (sprite->w_pix - TILE_W) / 2;
                dest.y -= (sprite->h_pix - TILE_H);
        }

        frame = (frame + sprite_ticks) % sprite->n_frames;
        frame += sprite->sequence * sprite->n_frames;

        if (sprite->faded) {
                sprite_blit_faded(sprite->rsurf->surf, &sprite->frames[frame],
                                  &dest);
        } else {
                screen_blit(sprite->rsurf->surf, &sprite->frames[frame], &dest);
        }

}

static void _sprite_fin(void *vptr)
{
        /* Internal destructor. */

        struct sprite *sprite = (struct sprite *) vptr;
        if (sprite->tag)
                free(sprite->tag);
        if (sprite->frames)
                free(sprite->frames);
        if (sprite->decor)
                sprite_deref(sprite->decor);
        rsurf_unref(sprite->rsurf);
}

static struct sprite *_sprite_alloc(int frames, int facings)
{
        /* Internal constructor. */

        struct sprite *sprite;

        sprite = MEM_ALLOC_TYPE(struct sprite, _sprite_fin);

        sprite->n_frames = frames;
        sprite->facing = SPRITE_DEF_FACING;
        sprite->facings = facings;
        sprite->n_total_frames = (sprite->n_frames
                                  * (sprite->facings ?
                                     NUM_PLANAR_DIRECTIONS : 1));

        /* Allocate and initialize the rect structures which index into the
         * image. One rect per frame of animation. Note that 'facings' is a
         * bitmask, not a count. Sprites that don't have different facings
         * specify 'facings' as zero, so for these assume we'll want one
         * sequence of frames. Sprites that do support facings will need as
         * many sequences as there are directions supported by the game.
         */
        sprite->frames = (SDL_Rect *) calloc(sprite->n_total_frames,
                                             sizeof (SDL_Rect));

        return sprite;
}

void sprite_paint(struct sprite *sprite, int frame, int x, int y)
{
        while (sprite) {

                if (sprite->wave) {
                        sprite_paint_wave(sprite, frame, x, y);
                } else {
                        sprite_paint_normal(sprite, frame, x, y);
                }

                sprite = sprite->decor;
        }
}

static void sprite_on_tick(void *arg)
{
        /* Advance the animation frame. */

        /* XXX: convert TimeStop from a global to a local generic var like
         * sprite_disable/enable_ticks. */
        if (!TimeStop) {
                sprite_ticks++;
        }

        /* Call the watchers. */
        struct list *elem = sprite_watchers.next;
        while (elem != &sprite_watchers) {
                sprite_watcher_t *h = list_entry(elem, sprite_watcher_t, list);
                elem = elem->next;
                h->callback(h->arg);
        }
}

void *sprite_watch(void (*callback) (void *arg), void *arg)
{
        sprite_watcher_t *h = MEM_ALLOC_TYPE(sprite_watcher_t, NULL);
        h->callback = callback;
        h->arg = arg;
        list_add(&sprite_watchers, &h->list);
        return h;
}

void sprite_unwatch(void *handle)
{
        sprite_watcher_t *h = (sprite_watcher_t *) handle;
        list_remove(&h->list);
        mem_deref(h);
}


int sprite_init(void)
{
        list_init(&sprite_watchers);
        tick_watch(AnimationTicks, &sprite_on_tick, NULL);
        return 0;
}

int sprite_get_facing(struct sprite *sprite)
{
        return sprite->facing;
}

int sprite_set_facing(struct sprite *sprite, int facing)
{
        int bit, i;

        if (facing == SPRITE_DEF_FACING) {
                sprite->sequence = 0;
                return 0;
        }
        // facing supported?
        if ((sprite->facings & (1 << facing)) == 0) {
                dbg("warn: sprite_set_facing: facing=%d invalid for "
                    "sprite %s\n", facing, sprite->tag);
                return -1;
        }

        sprite->facing = facing;
        sprite->sequence = 0;

        // Find the sequence
        for (i = 0; i < facing; i++) {
                bit = (1 << i);
                if (sprite->facings & bit)
                        sprite->sequence++;
        }

        return 0;
}

int sprite_fade(struct sprite *sprite)
{
        sprite->faded = 1;
        return 0;
}

void sprite_unfade(struct sprite *sprite)
{
        sprite->faded = 0;
}

void sprite_zoom_out(int factor)
{
        sprite_zoom_factor *= factor;
}

extern void sprite_zoom_in(int factor)
{
        sprite_zoom_factor /= factor;
}

struct sprite *sprite_new(const char *tag, int frames, int index, int wave,
                          int facings, struct images *images)
{
        struct sprite *sprite;
        int col_width;
        int row_height;
        int i;
        int frame;
        int col;
        int row;

        /* Allocate it. */
        sprite = _sprite_alloc(frames, facings);
        if (!sprite)
                return 0;

        /* Dupe the tag if applicable. */
        if (tag) {
                if (!(sprite->tag = strdup(tag)))
                        goto abort;
        }

        /* Create a new refcounted surf. */
        if (!(sprite->rsurf = rsurf_new(images->images))) {
                goto abort;
        }

        /* Fill out the rest of the basic fields. */
        sprite->wave = ! !wave;
        sprite->w_pix = images->w;
        sprite->h_pix = images->h;

        /* Fill out the frames based on the index and image info. */
        col_width = (images->w + images->offx);
        row_height = (images->h + images->offy);
        for (i = 0, frame = index; i < sprite->n_total_frames; i++, frame++) {
                col = frame % images->cols;
                row = frame / images->cols;
                sprite->frames[i].x = col * col_width + images->offx;
                sprite->frames[i].y = row * row_height + images->offy;
                sprite->frames[i].w = images->w;
                sprite->frames[i].h = images->h;
        }

        return sprite;

      abort:
        sprite_deref(sprite);
        return NULL;
}

struct sprite *sprite_clone(struct sprite *orig, const char *tag)
{
        SDL_Rect *frames;

        /* Allocate it. */
        struct sprite *sprite = _sprite_alloc(orig->n_frames,
                                              orig->facings);
        if (!sprite) {
                return 0;
        }

        /* Remember the frames pointer before we wipe it out with the copy. */
        frames = sprite->frames;

        /* Copy the sprite info. */
        memcpy(sprite, orig, sizeof (*orig));

        /* Copy the frames. */
        sprite->frames = frames;
        memcpy(sprite->frames, orig->frames,
               sprite->n_total_frames * sizeof (sprite->frames[0]));

        /* Bump the refcount on the surface. */
        sprite->rsurf->ref++;

        /* Dupe the tag if applicable. */
        if (tag) {
                sprite->tag = strdup(tag);
        } else if (orig->tag) {
                sprite->tag = strdup(orig->tag);
        }

        return sprite;
}

void sprite_append_decoration(struct sprite *base, struct sprite *decor)
{
        assert(base);
        while (base->decor) {
                base = base->decor;
        }
        base->decor = sprite_clone(decor, decor->tag);
}

char *sprite_get_tag(struct sprite *sprite)
{
        return sprite->tag;
}

int sprite_is_faded(struct sprite *sprite)
{
        return sprite->faded;
}

int sprite_can_face(struct sprite *sprite, int facing)
{
        return (sprite->facings & (1 << facing));
}

/* sprite_save - save to file for reload. */
void sprite_save(struct sprite *sprite, struct save *save)
{
        /* For simple sprites just save the tag. */
        if (!sprite->decor) {
                assert(sprite->tag);
                save->write(save, "%s ; sprite\n", sprite->tag);
                return;
        }

        /* For composite sprites */
        save->write(save, ("(mk-composite-sprite (list "));
        while (sprite) {
                assert(sprite->tag);
                save->append(save, "%s ", sprite->tag);
                sprite = sprite->decor;
        }
        save->append(save, ")) ; composite sprite\n");
}

void sprite_strip_decorations(struct sprite *sprite)
{
        if (sprite->decor) {
                /* Decoration sprites are always single-referenced clones, so
                 * blow them away when they're stripped. This will recursively
                 * delete all the trailing decor sprites. */
                sprite_deref(sprite->decor);
                sprite->decor = 0;
        }
}

void sprite_blit_over(struct sprite *dest, struct sprite *src)
{
        int i = 0;

        /* Check preconditions. */
        assert(dest->w_pix == src->w_pix);
        assert(dest->h_pix == src->h_pix);
        assert(dest->n_total_frames == src->n_total_frames);

        /* Clone the destination sprite's surface before changing it. */
        if (sprite_clone_and_replace_rsurf(dest))
                return;

        /* For each frame... */
        for (i = 0; i < dest->n_total_frames; i++) {

                /* Blit the source over the destination. */
                sprite_custom_blit(src->rsurf->surf, &src->frames[i],
                                   dest->rsurf->surf, &dest->frames[i]);

        }
}

int sprite_num_frames(struct sprite *sprite)
{
        return sprite->n_frames;
}

int sprite_facings_mask(struct sprite *sprite)
{
        return sprite->facings;
}

void sprite_paint_direct(struct sprite *sprite, int frame, SDL_Rect * dest)
{
        screen_blit(sprite->rsurf->surf, &sprite->frames[frame], dest);
}

void sprite_deref(struct sprite *sprite)
{
        mem_deref(sprite);
}
