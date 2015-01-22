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

#ifndef sprite_h
#define sprite_h

#include <SDL.h>

/* The default facing is used by objects that need to save their facing. */
#define SPRITE_DEF_FACING -1

/**
 * Module init.
 */
int sprite_init(void);

/**
 * Add/remove callbacks to run whenever the sprite flips animations
 * frames. sprite_watch returns a handle to be used with sprite_unwatch.
 */
void *sprite_watch(void (*callback)(void *arg), void *arg);
void sprite_unwatch(void *handle);

/**
 * Constructors and deref.
 */
struct sprite *sprite_new(
        const char *tag, int frames, int index, int wave, int facings,
        struct images *image);
struct sprite *sprite_clone(struct sprite *orig, const char *new_tag);
void sprite_deref(struct sprite *sprite);

/**
 * Accessors.
 */
char *sprite_get_tag(struct sprite *sprite);
int sprite_num_frames(struct sprite *sprite);

/**
 * Saver.
 */
void sprite_save(struct sprite *sprite, struct save *save);

/**
 * Returns the bit mask representing the facings supported by this sprite.
 */
int sprite_facings_mask(struct sprite *sprite);
int sprite_can_face(struct sprite *sprite, int facing);

/**
 * This is a mistake: keeping facing info in the sprite instead of in the
 * object that refers to it.
 */
int sprite_set_facing(struct sprite *sprite, int direction);
int sprite_get_facing(struct sprite *sprite);

/**
 * Paint methods. The difference between these two is that paint_frame does not
 * use the global animation counter to offset the frame.
 */
void sprite_paint(struct sprite *sprite, int frame, int x, int y);
void sprite_paint_frame(struct sprite *sprite, int frame, int x, int y);

/**
 * Misc special effects. These modify the target sprite.
 */
int sprite_fade(struct sprite *sprite);
void sprite_unfade(struct sprite *sprite);
int sprite_is_faded(struct sprite *sprite);
void sprite_zoom_out(int factor);
void sprite_zoom_in(int factor);

/**
 * Add/remove "decorations" from the sprite. Decorations are other sprites
 * which are blitted over (in sequence) the base sprite in the sprite_paint
 * methods.
 */
void sprite_append_decoration(struct sprite *sprite,
				     struct sprite *decor);
void sprite_strip_decorations(struct sprite *sprite);

/**
 * Blit one sprite over another. The images of the destination sprite will be
 * copied and then modified by the blit, so you don't have to worry about other
 * sprites that refer to the same images. The two sprites should have the same
 * number of frames and the same dimensions or the results are not defined. The
 * modification will not be saved with the game, so it needs to be redone at
 * load time.
 */
void sprite_blit_over(struct sprite *dest, struct sprite *src);

/**
 * Not sure this is really necessary. It's called by kern_screen_draw_sprite,
 * but why can't it just use the ULC of dest and call sprite_paint?
 */
void sprite_paint_direct(struct sprite *sprite, int frame, SDL_Rect * dest);

#endif
