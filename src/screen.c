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
#include "screen.h"
#include "common.h"
#include "ascii.h"
#include "glyph.h"
#include "sprite.h"
#include "status.h"
#include "foogod.h"
#include "cfg.h"
#include "images.h"
#include "nazghul.h"            // for FullScreenMode

#include <png.h>
#include <unistd.h>
#include <stdarg.h>
#include <assert.h>

#include <SDL_image.h>

#define N_SHADERS 3
#define MAX_SHADER (N_SHADERS - 1)
#define SHADER_W SCREEN_W
#define SHADER_H SCREEN_H
#define HIGHLIGHT_THICKNESS 2

/* Frame image indices */
#define FRAME_ULC  0
#define FRAME_TD   1
#define FRAME_URC  2
#define FRAME_ENDT 3
#define FRAME_TR   4
#define FRAME_TX   5
#define FRAME_TL   6
#define FRAME_VERT 7
#define FRAME_LLC  8
#define FRAME_TU   9
#define FRAME_LRC  10
#define FRAME_ENDD 11
#define FRAME_ENDL 12
#define FRAME_HORZ 13
#define FRAME_ENDR 14
#define FRAME_DOT  15
#define FRAME_NUM_SPRITES 16

/* Enable this to dump surfaces and video info */
#ifndef SCREEN_DEBUG
#define SCREEN_DEBUG 1
#endif

static SDL_Surface *Screen;
static SDL_Texture *ScreenTexture;
static SDL_Window *Window;
static SDL_Renderer *Renderer;
static SDL_Surface *Shaders[N_SHADERS];
static SDL_Surface *Highlight;
static struct sprite *FrameSprites[FRAME_NUM_SPRITES];
static int Zoom;
static char screen_buf[128];
static struct glyph_formatter *screen_gf = NULL;

Uint32 Black;
Uint32 Blue;
Uint32 White;
Uint32 Green;
Uint32 Red;
Uint32 Yellow;
Uint32 Cyan;
Uint32 Magenta;
Uint32 Gray;

Uint32 TextRed;
Uint32 TextGreen;
Uint32 TextBlue;
Uint32 TextYellow;
Uint32 TextCyan;
Uint32 TextMagenta;

SDL_Color fontWhite = { 0xff, 0xff, 0xff, 0x00 };
SDL_Color fontBlack = { 0, 0, 0, 0 };

static void scaled_blit(SDL_Surface * source, SDL_Rect * from,
                        SDL_Surface * dest, SDL_Rect * to);

void screen_initColors(void)
{

        Black = SDL_MapRGB(Screen->format, 0x00, 0x00, 0x00);
        White = SDL_MapRGB(Screen->format, 0xff, 0xff, 0xff);
        Red = SDL_MapRGB(Screen->format, 0xff, 0x00, 0x00);
        TextRed = SDL_MapRGB(Screen->format, 0xff, 0x99, 0x99);
        Green = SDL_MapRGB(Screen->format, 0x00, 0xff, 0x00);
        TextGreen = SDL_MapRGB(Screen->format, 0x99, 0xff, 0x99);
        Blue = SDL_MapRGB(Screen->format, 0x00, 0x00, 0xff);
        TextBlue = SDL_MapRGB(Screen->format, 0x99, 0x99, 0xff);
        Yellow = SDL_MapRGB(Screen->format, 0xff, 0xff, 0x00);
        TextYellow = SDL_MapRGB(Screen->format, 0xff, 0xff, 0x99);
        Cyan = SDL_MapRGB(Screen->format, 0x00, 0xff, 0xff);
        TextCyan = SDL_MapRGB(Screen->format, 0x99, 0xff, 0xff);
        Magenta = SDL_MapRGB(Screen->format, 0xff, 0xff, 0x00);
        TextMagenta = SDL_MapRGB(Screen->format, 0xff, 0x99, 0xff);
        Gray = SDL_MapRGB(Screen->format, 0x80, 0x80, 0x80);
}

void dump_SDL_RenderInfo(SDL_RendererInfo * fmt) {
	printf("Render Info:\n");
        printf("               name: %s\n", fmt->name);
        printf("              flags: \n");
        if (fmt->flags & SDL_RENDERER_SOFTWARE)
               printf("     SDL_RENDERER_SOFTWARE\n");
        if (fmt->flags & SDL_RENDERER_ACCELERATED)
               printf("     SDL_RENDERER_ACCELERATED\n");
        if (fmt->flags & SDL_RENDERER_PRESENTVSYNC)
               printf("     SDL_RENDERER_PRESENTVSYNC\n");
        if (fmt->flags & SDL_RENDERER_TARGETTEXTURE)
               printf("     SDL_RENDERER_TARGETTEXTURE\n");
        printf("    texture_formats: %d\n", fmt->num_texture_formats);
	for( Uint32 i = 0; i < fmt->num_texture_formats; i++ )
        {
               printf("     %s\n", SDL_GetPixelFormatName(fmt->texture_formats[i]));
        }
        printf("  max_texture_width: %d\n", fmt->max_texture_width);
        printf(" max_texture_height: %d\n", fmt->max_texture_height);
}


void dump_SDL_PixelFormat(SDL_PixelFormat * fmt)
{
        printf("Pixel Format:\n");
        printf("     palette: %p\n", fmt->palette);
        printf("BitsPerPixel: %d\n", fmt->BitsPerPixel);
        printf("       Rmask: 0x%x\n", fmt->Rmask);
        printf("       Gmask: 0x%x\n", fmt->Gmask);
        printf("       Bmask: 0x%x\n", fmt->Bmask);
        printf("       Amask: 0x%x\n", fmt->Amask);
        printf("      Rshift: %d\n", fmt->Rshift);
        printf("      Gshift: %d\n", fmt->Gshift);
        printf("      Bshift: %d\n", fmt->Bshift);
        printf("      Ashift: %d\n", fmt->Ashift);
        printf("       Rloss: %d\n", fmt->Rloss);
        printf("       Gloss: %d\n", fmt->Gloss);
        printf("       Bloss: %d\n", fmt->Bloss);
        printf("       Aloss: %d\n", fmt->Aloss);
}

void dump_SDL_Surface(SDL_Surface * surf)
{
        printf("Surface Info:\n");
        printf("     flags:\n");
        if (surf->flags & SDL_SWSURFACE)
                printf("  SDL_SWSURFACE\n");
        if (surf->flags & SDL_PREALLOC)
                printf("  SDL_PREALLOC\n");
        if (surf->flags & SDL_RLEACCEL)
                printf("  SDL_RLEACCEL\n");
        if (surf->flags & SDL_DONTFREE)
                printf("  SDL_DONTFREE\n");
        if (surf->flags & SDL_SIMD_ALIGNED)
                printf("  SDL_SIMD_ALIGNED\n");
        printf("         w: %d\n", surf->w);
        printf("         h: %d\n", surf->h);
        printf("     pitch: %d\n", surf->pitch);
        printf("    pixels: %p\n", surf->pixels);
        printf(" clip_rect: [%d %d %d %d]\n",
               surf->clip_rect.x,
               surf->clip_rect.y, surf->clip_rect.w, surf->clip_rect.h);
        printf("  refcount: %d\n", surf->refcount);
        dump_SDL_PixelFormat(surf->format);

}

void screen_initScreen(void)
{
        Uint32 flags = SDL_WINDOW_SHOWN;
        const int SCREEN_BPP = 32;

        if (SDL_Init(SDL_INIT_VIDEO) < 0) {
                perror_sdl("SDL_Init");
                exit(-1);
        }
        atexit(SDL_Quit);

        if (FullScreenMode) {
                flags |= SDL_WINDOW_FULLSCREEN;
        }

        Window = SDL_CreateWindow(APPLICATION_NAME,
                          SDL_WINDOWPOS_UNDEFINED,
                          SDL_WINDOWPOS_UNDEFINED,
                          SCREEN_W, SCREEN_H,
                          flags);

        if (!Window) {
               perror_sdl("SDL_CreateWindow");
                exit(-1);
        }

        Renderer = SDL_CreateRenderer(Window, -1, 0 );

        if (!Renderer) {
               perror_sdl("SDL_CreateRenderer");
                exit(-1);
        }

        ScreenTexture = SDL_CreateTexture(Renderer,
                               SDL_PIXELFORMAT_RGB888,
                               SDL_TEXTUREACCESS_STREAMING,
                               SCREEN_W, SCREEN_H);

        if (!ScreenTexture) {
               perror_sdl("SDL_CreateTexture");
                exit(-1);
        }

        Screen = SDL_CreateRGBSurface(0,
                       SCREEN_W, SCREEN_H,
                       SCREEN_BPP, 0, 0, 0, 0);

        if (!Screen) {
                perror_sdl("SDL_CreateRGBSurface");
                exit(-1);
        }

        if (SCREEN_DEBUG) {
                SDL_RendererInfo info;
                if (SDL_GetRendererInfo(Renderer, &info) != 0)
                {
                       perror_sdl("SDL_GetRendererInfo");
                } else {
                        dump_SDL_RenderInfo(&info);
                }

                static int display_in_use = 0;
                int i, display_mode_count;
                SDL_DisplayMode mode;
                Uint32 f;

                SDL_Log("SDL_GetNumVideoDisplays(): %i", SDL_GetNumVideoDisplays());

                display_mode_count = SDL_GetNumDisplayModes(display_in_use);
                if (display_mode_count < 1) {
                        SDL_Log("SDL_GetNumDisplayModes failed: %s", SDL_GetError());
                        exit(-1);
                }
                SDL_Log("SDL_GetNumDisplayModes: %i", display_mode_count);

                for (i = 0; i < display_mode_count; ++i) {
                        if (SDL_GetDisplayMode(display_in_use, i, &mode) != 0) {
                                SDL_Log("SDL_GetDisplayMode failed: %s", SDL_GetError());
                                exit(-1);
                        }
                        f = mode.format;

                        SDL_Log("Mode %i\tbpp %i\t%s\t%i x %i",
                                i, SDL_BITSPERPIXEL(f),
                                SDL_GetPixelFormatName(f),
                                mode.w, mode.h);
                }
        }
}

void screen_fade_surface(SDL_Surface * surf, int fade_level)
{
        int x, y;
        Uint8 *pix;
        Uint32 trans;
        int base;
        int toggle;

        assert(surf->format->BitsPerPixel == 8);

        if (fade_level == 0)
                return;

        pix = (Uint8 *) surf->pixels;
        SDL_GetColorKey(surf, &trans);

        for (y = 0; y < surf->h; y++) {
                base = y * surf->pitch;
                toggle = y % 2;
                for (x = 0; x < surf->w; x++) {
                        int i = base + x;
                        if (toggle) {
                                if (pix[i] != (Uint8) trans) {
                                        toggle = 0;
                                        pix[i] = (Uint8) trans;
                                }
                        } else if (pix[i] != trans) {
                                toggle = 1;
                        }
                }               // for (x)
        }                       // for (y)

        assert(surf->format->BitsPerPixel == 8);

        screen_fade_surface(surf, fade_level - 1);
}

static SDL_Surface *create_shader(int fade_level)
{
        SDL_Surface *shader;

        shader = screen_create_surface(SHADER_W, SHADER_H);
        if (shader == NULL)
                return NULL;

        SDL_FillRect(shader, NULL, SDL_MapRGBA(shader->format, 0, 0, 0, 0));

        if (shader->format->palette != NULL) {
                SDL_LockSurface(shader);
                screen_fade_surface(shader, fade_level);
                SDL_UnlockSurface(shader);
        }

        return shader;
}

void screen_initShader(void)
{
        int n, i;

        n = (Screen->format->BitsPerPixel == 8) ? N_SHADERS : 1;

        for (i = 0; i < n; i++) {
                Shaders[i] = create_shader(i);
        }
}

void screen_initHighlight(void)
{
        Highlight = screen_create_surface(SHADER_W, SHADER_H);
        assert(Highlight != NULL);

        SDL_FillRect(Highlight, NULL,
                     SDL_MapRGBA(Highlight->format, 255, 255, 255, 0));

        if (Highlight->format->palette != NULL) {
                SDL_LockSurface(Highlight);
                screen_fade_surface(Highlight, 4);
                SDL_UnlockSurface(Highlight);
        }
}

static void screen_initFrame(void)
{
        int i;
        char *fname = cfg_get("frame-image-filename");
        struct images *ss_frame = 0;

        if (!fname) {
                warn("No frame image filename!");
                return;
        }

        memset(FrameSprites, 0, sizeof (FrameSprites));

        ss_frame = images_new(0, 16, 16, 4, 4, 0, 0, fname);
        assert(ss_frame);

        for (i = 0; i < FRAME_NUM_SPRITES; i++) {
                FrameSprites[i] = sprite_new(0, 1, i, 0, 0, ss_frame);
                assert(FrameSprites[i]);
        }
}

int screen_init(void)
{
        if (!(screen_gf = glyph_formatter_alloc())) {
                return -1;
        }

        screen_initScreen();
        screen_initColors();
        screen_initShader();
        screen_initHighlight();
        screen_initFrame();
        Zoom = 1;

        return 0;
}

void screen_erase(SDL_Rect * rect)
{
        SDL_FillRect(Screen, rect, Black);
}

void screen_fill(SDL_Rect * rect, Uint32 color)
{
        rect->w /= Zoom;
        rect->h /= Zoom;
        SDL_FillRect(Screen, rect, color);
}

void screen_update(SDL_Rect * rect)
{
        SDL_UpdateTexture(ScreenTexture, NULL, Screen->pixels, Screen->pitch);
        SDL_RenderClear(Renderer);
        SDL_RenderCopy(Renderer, ScreenTexture, NULL, NULL);
        SDL_RenderPresent(Renderer);
}

/* bpp-independent macro to test if a pixel is magenta */
#define isTransparent(ff,pp) \
        (((ff)->Rmask&(pp))==(ff)->Rmask \
          && ((ff)->Gmask&(pp))==0 \
          && ((ff)->Bmask&(pp))==(ff)->Bmask)

static void scaled_blit_32bpp(SDL_Surface * source, SDL_Rect * from,
                              SDL_Surface * dest, SDL_Rect * to,
                              int spitch, int dpitch)
{
        int dx, dy, di, sx, sy, si;
        Uint32 *d, *s;

        d = (Uint32 *) dest->pixels;
        s = (Uint32 *) source->pixels;

        for (dy = 0; dy < to->h; dy++) {
                sy = dy * Zoom;
                for (dx = 0; dx < to->w; dx++) {
                        sx = dx * Zoom;
                        di = (dy + to->y) * dpitch + (dx + to->x);
                        si = (sy + from->y) * spitch + (sx + from->x);
                        if (!isTransparent(dest->format, s[si]))
                                d[di] = s[si];
                }               // for (dx)
        }                       // for (dy)
}

static void scaled_blit_16bpp(SDL_Surface * source, SDL_Rect * from,
                              SDL_Surface * dest, SDL_Rect * to,
                              int spitch, int dpitch)
{
        int dx, dy, di, sx, sy, si;
        Uint16 *d, *s;

        d = (Uint16 *) dest->pixels;
        s = (Uint16 *) source->pixels;

        for (dy = 0; dy < to->h; dy++) {
                sy = dy * Zoom;
                for (dx = 0; dx < to->w; dx++) {
                        sx = dx * Zoom;
                        di = (dy + to->y) * dpitch + (dx + to->x);
                        si = (sy + from->y) * spitch + (sx + from->x);
                        if (!isTransparent(dest->format, s[si]))
                                d[di] = s[si];
                }               // for (dx)
        }                       // for (dy)
}

static void scaled_blit_8bpp(SDL_Surface * source, SDL_Rect * from,
                             SDL_Surface * dest, SDL_Rect * to,
                             int spitch, int dpitch)
{
        int dx, dy, di, sx, sy, si;
        Uint8 *d, *s;

        d = (Uint8 *) dest->pixels;
        s = (Uint8 *) source->pixels;

        for (dy = 0; dy < to->h; dy++) {
                sy = dy * Zoom;
                for (dx = 0; dx < to->w; dx++) {
                        sx = dx * Zoom;
                        di = (dy + to->y) * dpitch + (dx + to->x);
                        si = (sy + from->y) * spitch + (sx + from->x);
                        d[di] = s[si];
                }               // for (dx)
        }                       // for (dy)
}

/* scale_then_blit_normal -- cheesy hack to support scaled blitting of
 * incompatible surface types. This blits the source to a temporary compatible
 * surface using the scaled_blit function, then blits the tmp surface to the
 * screen with Zoom=1. Inefficient but functional. */
static void scale_then_blit_normal(SDL_Surface * source, SDL_Rect * from,
                                   SDL_Surface * dest, SDL_Rect * to)
{
        SDL_Surface *tmp = 0;
        SDL_Rect rect;
        int o_zoom = Zoom;

        /* Create a temporary surface for the scaled blit which has the same
         * format as the source. */
        tmp = SDL_CreateRGBSurface(source->flags,
                                   from->w / Zoom, from->h / Zoom,
                                   source->format->BitsPerPixel,
                                   source->format->Rmask,
                                   source->format->Gmask,
                                   source->format->Bmask,
                                   source->format->Amask);
        if (!tmp) {
                perror_sdl("SDL_CreateRGBSurface");
                return;
        }

        /* Setup a rect for the tmp surface. */
        rect.x = 0;
        rect.y = 0;
        rect.w = to->w;
        rect.h = to->h;

        /* Do a scaled_blit from the source to the temporary surface. */
        scaled_blit(source, from, tmp, &rect);

        /* Do a normal blit from the tmp surface to the final dest, temporarily
         * setting Zoom factor to 1 to prevent another call into
         * scaled_blit(). */
        o_zoom = Zoom;
        Zoom = 1;
        screen_blit(tmp, &rect, to);
        Zoom = o_zoom;

        /* Free the tmp surface. */
        SDL_FreeSurface(tmp);
}

static void scaled_blit(SDL_Surface * source, SDL_Rect * from,
                        SDL_Surface * dest, SDL_Rect * to)
{
        int dpitch, spitch;

        assert(Zoom > 0);

        /* This is not a general-purpose blitting routine. If the source and
         * destination surfaces don't have the same format then use a hack to
         * workaround it. */
        if (source->format->BitsPerPixel != dest->format->BitsPerPixel
            || source->format->Amask != dest->format->Amask) {
                scale_then_blit_normal(source, from, dest, to);
                return;
        }

        to->w /= Zoom;
        to->h /= Zoom;

        dpitch = dest->pitch / dest->format->BytesPerPixel;
        spitch = source->pitch / source->format->BytesPerPixel;

        if (SDL_LockSurface(dest) < 0)
                return;

        switch (dest->format->BitsPerPixel) {
        case 32:
                scaled_blit_32bpp(source, from, dest, to, spitch, dpitch);
                break;
        case 16:
                scaled_blit_16bpp(source, from, dest, to, spitch, dpitch);
                break;
        case 8:
                scaled_blit_8bpp(source, from, dest, to, spitch, dpitch);
                break;
        default:
                assert(0);
                break;
        }

        SDL_UnlockSurface(dest);
}

void screen_blit(SDL_Surface * source, SDL_Rect * from, SDL_Rect * to)
{
        /* Clipping is really only needed for wave sprites right now. If the
         * following proves to be too expensive on slow machines... */
        if (to) {
                SDL_Rect _to = *to;
                SDL_SetClipRect(Screen, &_to);
                if (Zoom > 1) {

                        // Clients are allowed to pass a NULL from rect,
                        // indicating they want to blit the whole source
                        // area. But the scaled blits require a non-NULL
                        // from rect.
                        SDL_Rect _from;
                        if (from == NULL) {
                                _from.x = 0;
                                _from.y = 0;
                                _from.w = source->w;
                                _from.h = source->h;
                                from = &_from;
                        }

                        scaled_blit(source, from, Screen, &_to);
                } else {
                        if (SDL_BlitSurface(source, from, Screen, &_to) < 0)
                                perror_sdl("SDL_BlitSurface");
                }
                SDL_SetClipRect(Screen, 0);
        } else {
                SDL_SetClipRect(Screen, to);
                if (SDL_BlitSurface(source, from, Screen, NULL) < 0)
                        perror_sdl("SDL_BlitSurface");
                SDL_SetClipRect(Screen, 0);
        }
}

int screen_width(void)
{
        return Screen->w;
}

int screen_height(void)
{
        return Screen->h;
}

SDL_PixelFormat *screen_format(void)
{
        return Screen->format;
}

void screen_flash(SDL_Rect * rect, int mdelay, Uint32 color)
{
        screen_fill(rect, color);
        screen_update(rect);
        //usleep(mdelay * 1000);
        SDL_Delay(mdelay);
}

void screen_print_glyph_buf(SDL_Rect * rect, int flags, glyph_buf_t * gbuf)
{
        int i;
        int x = rect->x;
        int y = rect->y;
        int len, stop;

        len = glyph_buf_len(gbuf);
        stop = rect->x + (rect->w * ASCII_W);

        /* If painting on the border then first fill the line with the border
         * image. */
        if (flags & SP_ONBORDER) {
                for (x = rect->x; x < rect->x + rect->w; x += BORDER_W) {
                        sprite_paint(FrameSprites[FRAME_HORZ], 0, x, rect->y);
                }
        }

        /* Calculate offset for center and right-justified cases */
        if (flags & SP_CENTERED) {
                int w = len * ASCII_W;
                if (w > rect->w) {
                        w = rect->w;
                }
                x = (rect->w - w) / 2 + rect->x;
        } else if (flags & SP_RIGHTJUSTIFIED) {
                int w = len * ASCII_W;
                if (w > rect->w) {
                        w = rect->w;
                }
                x = (rect->w - w) + rect->x;
        }

        /* If painting on the border, then paint the right stub 
         * to the left of the text. */
        if (flags & SP_ONBORDER) {
                sprite_paint(FrameSprites[FRAME_ENDR], 0, x - BORDER_W,
                             rect->y);
        }

        /* Paint the characters until we run out or hit the end of the
         * region. */
        glyph_buf_iter_t *gbi = glyph_buf_iter_alloc(gbuf);
        for (i = 0; i < len && x < stop; i++) {
                ascii_paint_glyph(glyph_buf_iter_next(gbi), x, y, Screen);
                x += ASCII_W;
        }
        glyph_buf_iter_deref(gbi);

        /* If painting on the border, then paint the left stub 
         * to the right of the text. */
        if (flags & SP_ONBORDER) {
                sprite_paint(FrameSprites[FRAME_ENDL], 0, x, rect->y);
        }
}

void screen_print(SDL_Rect * rect, int flags, const char *fmt, ...)
{
        va_list args;

        /* Print the string to a buffer. */
        va_start(args, fmt);
        vsnprintf(screen_buf, sizeof (screen_buf), fmt, args);
        va_end(args);

        /* Convert it to a glyph string for printing */
        glyph_buf_t *gbuf = glyph_buf_alloc_and_format(screen_gf, screen_buf);
        screen_print_glyph_buf(rect, flags, gbuf);
        glyph_buf_deref(gbuf);
}

void screen_repaint_frame(void)
{
        int i;

        // First draw the top and bottom horizontal bars. Leave gaps for the
        // sky and wind windows. Originally I went ahead and painted over them
        // here, relying on their update routines to black out their
        // backgrounds. But when I started using the tall/short mode for the
        // status window I found that this was no longer good enough. The
        // backgrounds of these windows tended to flash when switching mode.

        // Draw the top bar from the top left corner to the sky window.
        for (i = 0; i < SKY_X - BORDER_W; i += BORDER_W)
                sprite_paint(FrameSprites[FRAME_HORZ], 0, i, 0);

        // Draw the top bar from the sky window to the left edge of the status
        // window's title.
        for (i = SKY_X + SKY_W + BORDER_W; i < STAT_X; i += BORDER_W)
                sprite_paint(FrameSprites[FRAME_HORZ], 0, i, 0);

        // Draw the bottom of the map from the left edge to the wind window.
        for (i = 0; i < (int) (WIND_X - BORDER_W); i += BORDER_W)
                sprite_paint(FrameSprites[FRAME_HORZ], 0, i, MAP_X + MAP_H);

        // Draw the bottom of the map from the wind window to the left edge of
        // the console window.
        for (i = WIND_X + WIND_W + BORDER_W; i < CONS_X - BORDER_W;
             i += BORDER_W)
                sprite_paint(FrameSprites[FRAME_HORZ], 0, i, MAP_X + MAP_H);

        // Draw the bar across the bottom of the screen.
        for (i = 0; i < SCREEN_W; i += BORDER_W)
                sprite_paint(FrameSprites[FRAME_HORZ], 0, i,
                             SCREEN_H - BORDER_H);

        // Next draw the bottom of the status and food/gold window.
        for (i = (MAP_X + MAP_W); i < SCREEN_W; i += BORDER_W) {
                sprite_paint(FrameSprites[FRAME_HORZ], 0, i,
                             STAT_Y + status_get_h());
                sprite_paint(FrameSprites[FRAME_HORZ], 0, i,
                             foogod_get_y() + FOOGOD_H);
        }

        // Next rough in all the vertical lines.
        for (i = 0; i < SCREEN_H; i += BORDER_H) {
                sprite_paint(FrameSprites[FRAME_VERT], 0, 0, i);
                sprite_paint(FrameSprites[FRAME_VERT], 0, MAP_X + MAP_W, i);
                sprite_paint(FrameSprites[FRAME_VERT], 0, SCREEN_W -
                             BORDER_W, i);
        }

        // Now paint the four corner pieces
        sprite_paint(FrameSprites[FRAME_ULC], 0, 0, 0);
        sprite_paint(FrameSprites[FRAME_URC], 0, SCREEN_W - BORDER_W, 0);
        sprite_paint(FrameSprites[FRAME_LLC], 0, 0, SCREEN_H - BORDER_H);
        sprite_paint(FrameSprites[FRAME_LRC], 0, SCREEN_W - BORDER_W,
                     SCREEN_H - BORDER_H);

        // Then all the right-facing tee-joints
        sprite_paint(FrameSprites[FRAME_TR], 0, 0, MAP_Y + MAP_H);
        sprite_paint(FrameSprites[FRAME_TR], 0, MAP_X + MAP_W,
                     STAT_Y + status_get_h());
        sprite_paint(FrameSprites[FRAME_TR], 0, MAP_X + MAP_W,
                     foogod_get_y() + FOOGOD_H);

        // Then all the left-facing tee-joints
        sprite_paint(FrameSprites[FRAME_TL], 0, MAP_X + MAP_W, MAP_Y + MAP_H);
        sprite_paint(FrameSprites[FRAME_TL], 0, SCREEN_W - BORDER_W,
                     STAT_Y + status_get_h());
        sprite_paint(FrameSprites[FRAME_TL], 0, SCREEN_W - BORDER_W,
                     foogod_get_y() + FOOGOD_H);

        // Then the downward and upward-facing tee-joints
        sprite_paint(FrameSprites[FRAME_TD], 0, MAP_X + MAP_W, 0);
        sprite_paint(FrameSprites[FRAME_TU], 0, MAP_X + MAP_W,
                     SCREEN_H - BORDER_H);

        // And then the stubs around the sky section
        sprite_paint(FrameSprites[FRAME_ENDR], 0, SKY_X - BORDER_W, 0);
        sprite_paint(FrameSprites[FRAME_ENDL], 0, SKY_X + SKY_W, 0);

        // And finally stubs around the wind section
        sprite_paint(FrameSprites[FRAME_ENDR], 0, WIND_X - BORDER_W,
                     MAP_X + MAP_H);
        sprite_paint(FrameSprites[FRAME_ENDL], 0, WIND_X + WIND_W,
                     MAP_X + MAP_H);

        // And some stubs around the status title section
        sprite_paint(FrameSprites[FRAME_ENDR], 0, STAT_X, 0);
        sprite_paint(FrameSprites[FRAME_ENDL], 0, STAT_X + STAT_W - BORDER_W,
                     0);

        screen_update(0);

}

SDL_Surface *screen_create_surface(int w, int h)
{
        SDL_Surface *surf = NULL;

        surf = SDL_CreateRGBSurfaceWithFormat(Screen->flags,
                                   w, h,
                                   Screen->format->BitsPerPixel,
                                   Screen->format->format);

        // surf->format->palette = Screen->format->palette;

        if (surf == NULL) {
                perror_sdl("SDL_CreateRGBSurfaceWithFormat");
                return NULL;
        }

        if (surf->format->palette) {
                SDL_SetColorKey(surf, SDL_TRUE,
                                SDL_MapRGB(surf->format, 0xFF, 0x00, 0xFF));
        }

        return surf;
}

void screen_copy(SDL_Rect * from, SDL_Rect * to, SDL_Surface * dest)
{
        if (SDL_BlitSurface(Screen, from, dest, to) < 0)
                perror_sdl("SDL_BlitSurface");

        assert(from);
        assert(dest);

        SDL_Rect _from = *from;

        if (Zoom > 1) {
                // Clients are allowed to pass a NULL 'to' rect,
                // indicating they want to blit the whole dest
                // area. But the scaled blits require a non-NULL
                // 'to' rect.
                SDL_Rect _to;
                if (to == NULL) {
                        _to.x = 0;
                        _to.y = 0;
                        _to.w = dest->w;
                        _to.h = dest->h;
                        to = &_to;
                }
                scaled_blit(Screen, &_from, dest, to);
        } else {
                if (SDL_BlitSurface(Screen, &_from, dest, to) < 0)
                        perror_sdl("SDL_BlitSurface");
        }
}

void screen_shade(SDL_Rect * area, unsigned char amount)
{
        SDL_Surface *shade;

        assert(area->w <= SHADER_W);
        assert(area->h <= SHADER_H);

        if (amount == 0)
                return;

        if (Screen->format->BitsPerPixel == 8) {
                shade = Shaders[MAX_SHADER - (amount * MAX_SHADER) / 255];
        } else {
                shade = Shaders[0];
                SDL_SetSurfaceAlphaMod(shade, amount);
                SDL_SetSurfaceBlendMode(shade, SDL_BLENDMODE_BLEND);
        }
        screen_blit(shade, NULL, area);
}

void screen_highlightColored(SDL_Rect * area, Uint32 color)
{
        SDL_Rect edge;

        // ---------------------------------------------------------------------
        // Top edge
        // ---------------------------------------------------------------------

        edge.x = area->x;
        edge.y = area->y;
        edge.w = area->w;
        edge.h = HIGHLIGHT_THICKNESS;

        screen_fill(&edge, color);

        // ---------------------------------------------------------------------
        // Bottom edge
        // ---------------------------------------------------------------------

        edge.x = area->x;
        edge.y = area->y + (area->h / Zoom) - HIGHLIGHT_THICKNESS;
        edge.w = area->w;
        edge.h = HIGHLIGHT_THICKNESS;

        screen_fill(&edge, color);

        // ---------------------------------------------------------------------
        // Left edge
        // ---------------------------------------------------------------------

        edge.x = area->x;
        edge.y = area->y;
        edge.w = HIGHLIGHT_THICKNESS;
        edge.h = area->h;

        screen_fill(&edge, color);

        // ---------------------------------------------------------------------
        // Right edge
        // ---------------------------------------------------------------------

        edge.x = area->x + (area->w / Zoom) - HIGHLIGHT_THICKNESS;
        edge.y = area->y;
        edge.w = HIGHLIGHT_THICKNESS;
        edge.h = area->h;

        screen_fill(&edge, color);
}

void screen_highlight(SDL_Rect * area)
{
        screen_highlightColored(area, White);
}

int screen_lock(void)
{
        return SDL_LockSurface(Screen);
}

void screen_unlock(void)
{
        SDL_UnlockSurface(Screen);
}

/* assumes the pixel value is gotten from screen_map_rgb() i.e. safe! */
void screen_set_pixel(int x, int y, Uint32 color)
{
        Uint8 *pix = (Uint8 *) (Screen->pixels);
        pix += y * Screen->pitch + x * Screen->format->BytesPerPixel;

        switch (Screen->format->BytesPerPixel) {
        case 4:
                *(Uint32 *) pix = (Uint32) color;
                break;
        case 2:
                *(Uint16 *) pix = (Uint16) color;
                break;
        case 1:
                *(Uint8 *) pix = (Uint8) color;
                break;
        default:
                assert(0);
                break;
        }
}

Uint32 screen_map_rgb(Uint8 red, Uint8 grn, Uint8 blu)
{
        return SDL_MapRGB(Screen->format, red, grn, blu);
}

void screen_zoom_out(int factor)
{
        if (factor)
                Zoom *= factor;
}

void screen_zoom_in(int factor)
{
        if (factor)
                Zoom /= factor;
}

void screen_capture(char *fname, SDL_Rect * rect)
{
        png_structp png_ptr = 0;
        png_infop info_ptr = 0;
        Uint32 *spix = 0;
        Uint8 *row_pointer = 0;
        int si, spitch;

        /* Open the destination file. */
        FILE *fp = fopen(fname, "wb");
        if (!fp) {
                return;
        }

        /* Setup PNG for writing. */
        png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
                                          (png_voidp) 0, 0, 0);
        if (!png_ptr) {
                goto done;
                return;
        }

        info_ptr = png_create_info_struct(png_ptr);
        if (!info_ptr) {
                goto done;
        }

        if (setjmp(png_jmpbuf(png_ptr))) {
                warn("screen_capture: PNG error!\n");
                goto done;
        }

        png_init_io(png_ptr, fp);

        /* Setup the image header. */
        png_set_IHDR(png_ptr, info_ptr,
                     MAP_W,
                     MAP_H,
                     8,
                     PNG_COLOR_TYPE_RGB,
                     PNG_INTERLACE_NONE,
                     PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

        /* Write the header. */
        png_write_info(png_ptr, info_ptr);

        /* TODO: if Screen is not in correct format, convert it to
         *       a suitable temp surface (maybe a row at the time?).
         */
        assert(Screen->format->BytesPerPixel == 4);
        /* Grab the screen pixels. */
        spix = (Uint32 *) Screen->pixels;
        spitch = Screen->pitch / Screen->format->BytesPerPixel;

        /* Allocate the row buffer. I copy pixels to an intermediate row buffer
         * so that I can handle different pixel formats (eg, RGBA vs ARGB,
         * etc). */
        row_pointer = (Uint8 *) malloc(rect->w * 3);
        assert(row_pointer);

        for (int y = 0; y < rect->h; y++) {

                /* Copy the SDL pixels into the intermediate buffer. PNG
                 * expects pixels in RGB order. */
                Uint8 *dpix = row_pointer;
                for (int x = 0; x < rect->w; x++) {
                        si = (y + rect->y) * spitch + (x + rect->x);
                        *dpix++ = ((spix[si] & Screen->format->Rmask)
                                   >> Screen->format->Rshift);
                        *dpix++ = ((spix[si] & Screen->format->Gmask)
                                   >> Screen->format->Gshift);
                        *dpix++ = ((spix[si] & Screen->format->Bmask)
                                   >> Screen->format->Bshift);
                }

                /* Write the row to PNG. */
                png_write_row(png_ptr, row_pointer);
        }

        png_write_end(png_ptr, 0);

      done:
        if (row_pointer) {
                free(row_pointer);
        }

        if (png_ptr) {
                png_destroy_write_struct(&png_ptr, &info_ptr);
        }

        fclose(fp);

}
