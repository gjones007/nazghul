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
#include "images.h"
#include "mem.h"
#include "screen.h"
#include "file.h"
#include "cfg.h"
#include "debug.h"
#include "screen.h"             // for screen_format()

#include <assert.h>
#include <SDL_image.h>
#include <stdlib.h>
#include <string.h>

void images_dump_surface(char *name, SDL_Surface * surf)
{
        printf("DUMP SURFACE ============================\n");
        printf("%s info:\n", name);
        printf("     w, h: %d %d\n", surf->w, surf->h);
        printf("    pitch: %d\n", surf->pitch);
        printf("clip_rect: [%d %d %d %d]\n", surf->clip_rect.x,
               surf->clip_rect.y, surf->clip_rect.w, surf->clip_rect.h);
        printf("   format:\n");
        printf("      palette: %s\n", surf->format->palette ? "yes" : "no");
        printf(" BitsPerPixel: %d\n", surf->format->BitsPerPixel);
        printf("BytesPerPixel: %d\n", surf->format->BytesPerPixel);
        printf("  R/G/B/Amask: 0x%x 0x%x 0x%x 0x%x\n",
               surf->format->Rmask, surf->format->Gmask, surf->format->Bmask,
               surf->format->Amask);
        printf(" R/G/B/Ashift: %d %d %d %d\n",
               surf->format->Rshift, surf->format->Gshift,
               surf->format->Bshift, surf->format->Ashift);
        printf("  R/G/B/Aloss: %d %d %d %d\n",
               surf->format->Rloss, surf->format->Gloss,
               surf->format->Bloss, surf->format->Aloss);
}

void images_fin(void *mem)
{
        struct images *images = (struct images *) mem;
        if (images->tag)
                free(images->tag);
        if (images->fname)
                free(images->fname);
        if (images->images)
                SDL_FreeSurface(images->images);
        if (images->faded)
                SDL_FreeSurface(images->faded);
}

int images_fade(struct images *images)
{

        if (images->faded)
                return 0;

        images->faded = screen_create_surface(images->images->w,
                                              images->images->h);
        if (images->faded == NULL)
                return -1;

        memcpy(images->faded->pixels, images->images->pixels,
               images->faded->pitch * images->faded->h);

        screen_fade_surface(images->faded, 1);
        return 0;
}

int images_convert2display(struct images *images)
{
        SDL_Surface *tmp;
        int imagesbits, screenbits;
        imagesbits = images->images->format->BitsPerPixel;
        screenbits = screen_format()->BitsPerPixel;

        /* Convert to video format for faster blitting
         * (*much* faster on certain display bit depths).
         * 8-bit surfaces are a special case in Nazghul though.
         */
        if (imagesbits != screenbits && imagesbits != 8) {

                if ((tmp = SDL_ConvertSurface(images->images, screen_format(), 0)) == NULL) {
                        err("SDL_ConvertSurface: %s", SDL_GetError());
                        return 0;
                }
                imagesbits = screenbits;
                SDL_FreeSurface(images->images);
                images->images = tmp;
        }

        /* Images which are saved with a transparency layer will have the
         * SDL_SRCALPHA flag set. Their alpha layer will be managed
         * automatically by SDL_BlitSurface(). For images without an alpha
         * layer, assume that magenta (RGB 0xff00ff) is the special color for
         * transparency. To correctly support transparent blitting we have to
         * set their color key to magenta and we have to convert them to match
         * the display format. */
	if (SDL_ISPIXELFORMAT_ALPHA(images->images->flags)) {

                if (imagesbits != screenbits) {
                        /* Convert to video format for faster blitting */
                        if ((tmp = SDL_ConvertSurface(images->images, screen_format(), 0)) == NULL) {
                                err("SDL_ConvertSurface: %s", SDL_GetError());
                                return 0;
                        }
                        SDL_FreeSurface(images->images);
                        images->images = tmp;
                }

                /* Make magenta the transparent color */
                if (SDL_SetColorKey(images->images, SDL_TRUE,
                                    SDL_MapRGB(images->images->format,
                                               0xff, 0x00, 0xff)) < 0) {
                        err("SDL_SetColorKey: %s", SDL_GetError());
                        return 0;
                }
        }
        return 1;
}

struct images *images_new(const char *tag, int w, int h, int rows, int cols,
                          int offx, int offy, const char *fname)
{
        struct images *images;
        char *filename;

        images = MEM_ALLOC_TYPE(struct images, images_fin);

        if (tag) {
                images->tag = strdup(tag);
                assert(images->tag);
        }

        images->fname = strdup(fname);
        assert(images->fname);

        images->w = w;
        images->h = h;
        images->offx = offx;
        images->offy = offy;
        images->rows = rows;
        images->cols = cols;

        filename = file_mkpath(cfg_get("include-dirname"), fname);
        if (filename) {
                images->images = IMG_Load(filename);
                free(filename);
        }
        if (!images->images) {
                // BUG: Mac OS X fails to load PNG images here, but GIF works
                // OK.  This could be a libPNG, libSDL, or libSDL_Image bug. In
                // the meantime, better error logging is helpful.
                err("IMG_Load() failed to load file '%s' because '%s'.\n",
                    fname, SDL_GetError());
                assert(false);
                // err("IMG_Load: %s", SDL_GetError()); The err() macro acted
                // strangely, emitting different error text than the above
                // would indicate.  Perhaps that is a clue to the bug?
                // 
                // Perhaps the err() macro, or the call above, 
                // differ from the nazghul-0.2.0 release and recent CVS?
        }

        if (images_convert2display(images)) {
                //images_dump_surface(fname, images->images);
                return images;
        }

        mem_deref(images);
        return NULL;
}
