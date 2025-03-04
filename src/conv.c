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
#include "Character.h"
//

#include "bitset.h"
#include "console.h"
#include "conv.h"
#include "cfg.h"
#include "event.h"
#include "cmdwin.h"
#include "common.h"
#include "Object.h"
#include <string.h>
#include "closure.h"
#include "log.h"
#include "scheme-private.h"     /* for keyword processing */
#include "session.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_KEYWORD_SZ 16

/**
 * Conversation structure.
 */
struct conv {
        struct closure *proc;   /* Closure which responds to keywords. */
        int ref;                /* Reference count. */
        int n_keywords;         /* Size of the keywords array. */
        char **keywords;        /* Keyword array. */
        bitset_t *marked;       /* Used keywords */
};

static int conv_room, conv_len;
static char conv_query[64], *conv_ptr;
static int conv_done;
static int conv_keyword_highlighting = 1;

/**
 * Keyhandler callback for processing player keystrokes while collecting the
 * query string. This stores typed characters into the conv_query buffer.
 *
 * @param kh is the keyhandler pushed for input handling.
 * @param key is the key the player just typed.
 * @returns 1 if the player is done entering the query, else 0.
 */
static int conv_get_player_query(struct KeyHandler *kh, int key, int keymod)
{
        int done = 0;

        if (console_handle_key(key, keymod)) {
                return 0;
        }

        switch (key) {
        case CANCEL:
                console_end();
                while (conv_ptr > conv_query) {
                        conv_ptr--;
                        *conv_ptr = 0;
                        cmdwin_pop();
                        conv_room++;
                }
                done = 1;
                break;
        case '\n':
                console_end();
                done = 1;
                break;
        case '\b':
                console_end();
                if (conv_ptr != conv_query) {
                        conv_ptr--;
                        *conv_ptr = 0;
                        conv_room++;
                        cmdwin_pop();
                }
                break;
        default:
                if (isprintable(key)
                    && conv_room) {
                        console_end();
                        cmdwin_push("%c", key);
                        *conv_ptr++ = key;
                        conv_room--;
                }
                break;
        }

        return done;
}

/**
 * Force the player query to a value. This is mainly done to start the

 * conversation off with the "HAIL" keyword.
 *
 * @param str is the query value to set.
 */
static void conv_set_query(const char *str)
{
        snprintf(conv_query, sizeof (conv_query) - 1, "%s", str);
        conv_query[MAX_KEYWORD_SZ] = 0;
        conv_len = strlen(conv_query);
}

/**
 * Internal destructor for a conversation struct.
 *
 * @param conv is the conversation to delete.
 */
static void conv_del(struct conv *conv)
{
        if (conv->proc) {
                closure_unref(conv->proc);
        }

        if (conv->keywords) {

                int i;

                for (i = 0; i < conv->n_keywords; i++) {
                        if (conv->keywords[i]) {
                                free(conv->keywords[i]);
                        }
                }

                free(conv->keywords);
        }

        if (conv->marked) {
                bitset_free(conv->marked);
        }

        free(conv);
}

/**
 * Callback used by the sort function to compare two values.
 *
 * @param p1 is one value.
 * @param p2 is the other value.
 * @returns 1 if p1>p2, 0 if p1==p2, -1 if p1<p2.
 */
static int conv_sort_cmp(const void *p1, const void *p2)
{
        return strcmp(*(char *const *) p1, *(char *const *) p2);
}

/**
 * Function used by the binary search in conv_is_keyword.
 *
 * @param wptr is the word to look for.
 * @param cptr is the keyword to compare against.
 * @returns 0 is cptr is a prefix of wptr; -1 if wptr < cptr; 1 if wptr > cptr.
 */
static int conv_prefix_cmp(char *wptr, char *cptr)
{
        int len = 0;

        while (*wptr && *cptr) {
                char wc = tolower(*wptr++);
                char cc = tolower(*cptr++);
                len++;
                int d = wc - cc;
                if (d != 0) {
                        return d;
                }
        }

        if (*cptr && !*wptr) {
                /* candidate is longer than word */
                return -1;
        }

        if (*wptr && !isspace(*wptr) && !ispunct(*wptr) && len < 4) {
                /* candidate is short but word is not */
                return 1;
        }

        return 0;
}

/**
 * Check if a word is a keyword.
 *
 * @conv is the conversation to check.
 * @word is the word to look for in the keywords.
 * @returns the index of the matching keyword, else -1 if not found.
 */
static int conv_lookup_keyword(struct conv *conv, char *word)
{
        int min = 0, max = conv->n_keywords, pivot;

        while (max > min) {

                /**
                 * apricot
                 *
                 * aardvark
                 * abacus
                 *
                 * zoo
                 * zebra
                 */

                pivot = ((max - min) / 2) + min;

                int d = conv_prefix_cmp(word, conv->keywords[pivot]);

                if (d > 0) {
                        /* word > pivot => search higher */
                        min = pivot + 1;
                } else if (d < 0) {
                        /* word < pivot => search lower */
                        max = pivot;
                } else {
                        return pivot;
                }
        }

        return -1;
}

/**
 * Return the keyword matching 'word' or NULL if not found.
 */
static const char *conv_get_matching_keyword(struct conv *conv, char *word)
{
        int i = conv_lookup_keyword(conv, word);
        return (i == -1) ? NULL : conv->keywords[i];
}

/**
 * Check if a word is a keyword and, if so, to mark it (marking is used to show
 * the player which keywords have already been used in a conversation).
 *
 * @conv is the conversation to check.
 * @word is the word to look for in the keywords.
 */
static void conv_mark_if_keyword(struct conv *conv, char *word)
{
        int index = conv_lookup_keyword(conv, word);
        if (index != -1) {
                bitset_set(conv->marked, index);
        }
}

static int conv_add_keyword(struct conv *conv, char *keyword, int key_index)
{
        assert(key_index < conv->n_keywords);
        if (!(conv->keywords[key_index] = strdup(keyword))) {
                warn("%s: strdup failed on %s", __FUNCTION__, keyword);
                return -1;
        }
        return 0;
}

static void conv_sort_keywords(struct conv *conv)
{
        qsort(conv->keywords, conv->n_keywords, sizeof (char *), conv_sort_cmp);
}

static void conv_highlight_keywords(struct conv *conv)
{
        int key_index = 0;
        scheme *sc = conv->proc->sc;
        pointer sym = conv->proc->code;

        assert(sc);
        assert(sym);

        if (sym == sc->NIL) {
                warn("%s: conv proc not a symbol", __FUNCTION__);
                return;
        }

        pointer ifc = sc->vptr->find_slot_in_env(sc, sc->envir, sym, 1);
        if (!scm_is_pair(sc, ifc)) {
                warn("%s: conv '%s' has no value", __FUNCTION__,
                     scm_sym_val(sc, sym));
                return;
        }

        pointer clos = scm_cdr(sc, ifc);
        if (!scm_is_closure(sc, clos)) {
                warn("%s: conv '%s' not a closure", __FUNCTION__,
                     scm_sym_val(sc, sym));
                return;
        }

        pointer env = scm_cdr(sc, clos);
        pointer vtable = scm_cdr(sc, scm_car(sc, scm_car(sc, env)));

        conv->n_keywords = scm_len(sc, vtable);

        if (!
            (conv->keywords =
             (char **) calloc(conv->n_keywords, sizeof (char *)))) {
                warn("%s: failed to allocate keyword array size %d",
                     __FUNCTION__, conv->n_keywords);
                return;
        }

        if (!(conv->marked = bitset_alloc(conv->n_keywords))) {
                warn("%s: failed to allocate bitset array size %d",
                     __FUNCTION__, conv->n_keywords);
                return;
        }

        while (scm_is_pair(sc, vtable)) {
                pointer binding = scm_car(sc, vtable);
                vtable = scm_cdr(sc, vtable);
                pointer var = scm_car(sc, binding);
                if (conv_add_keyword(conv, scm_sym_val(sc, var), key_index)) {
                        return;
                }
                key_index++;
        }

        conv_sort_keywords(conv);
}

struct conv *conv_new(struct closure *proc)
{
        struct conv *conv;

        if (!(conv = (struct conv *) calloc(1, sizeof (*conv)))) {
                return NULL;
        }

        conv->ref = 1;
        conv->proc = proc;
        closure_ref(proc);

        return conv;
}

void conv_save(struct conv *conv, struct save *save)
{
        closure_save(conv->proc, save);
}

void conv_unref(struct conv *conv)
{
        assert(conv->ref > 0);
        conv->ref--;
        if (!conv->ref) {
                conv_del(conv);
        }
}

void conv_ref(struct conv *conv)
{
        conv->ref++;
}

void conv_end()
{
        conv_done = 1;
}

void conv_enter(Object * npc, Object * pc, struct conv *conv)
{
        struct KeyHandler kh;

        assert(conv);

        npc->setSpeaking(true);

        if (!conv->keywords && conv_keyword_highlighting) {
                conv_highlight_keywords(conv);
        }

        /* If NPC initiates conversation, make sure we have a valid session
         * subject, else describe() will crash when determining if unknown NPC
         * is hostile or not. */
        if (!Session->subject) {
                Session->subject = (class Being *) pc;
        }

        log_banner("^c+yCONVERSATION^c-");

        session_run_hook(Session, conv_start_hook, "pp", pc, npc);

        conv_done = 0;
        kh.fx = conv_get_player_query;
        conv_set_query("hail");

        for (;;) {

                /* Truncate the query to 4 characters */
                //conv_query[4] = 0;

                conv_mark_if_keyword(conv, conv_query);

                /* If query was NAME, assume the NPC is now known */
                if (!strcasecmp(conv_query, "NAME")) {
                        ((class Character *) npc)->setKnown(true);
                }

                /* If the query matches a keyword prefix, expand/contract it to
                 * use the exact keyword, else just let if through as-is. */
                const char *keyword =
                    conv_get_matching_keyword(conv, conv_query);
                if (!keyword) {
                        keyword = conv_query;
                }

                /* Query the NPC */
                closure_exec(conv->proc, "ypp", keyword, npc, pc);

                if (conv_done)
                        break;

                /*** Setup for next query ***/

                memset(conv_query, 0, sizeof (conv_query));
                conv_room = sizeof (conv_query) - 1;
                conv_ptr = conv_query;

                cmdwin_clear();
                cmdwin_push("Say: ");

                /*** Get next query ***/

                eventPushKeyHandler(&kh);
                eventHandle();
                eventPopKeyHandler();

                conv_query[MAX_KEYWORD_SZ] = 0;
                conv_len = strlen(conv_query);
                if (!conv_len)
                        sprintf(conv_query, "bye");
                log_msg("^c+%c%s:^c- %s", CONV_PC_COLOR,
                        pc->getName(), conv_query);

                /*** Check if player ended conversation ***/

                if (Quit)
                        break;

                if (strlen(conv_query) == 0)
                        conv_set_query("BYE");

                if (!strcasecmp(conv_query, "BYE")) {
                        conv_end();
                }

        }

        cmdwin_clear();
        cmdwin_repaint();

        npc->setSpeaking(false);
        session_run_hook(Session, conv_end_hook, "pp", pc, npc);

}

int isprintable(int c)
{
        /* Looks like ctype's isprint() doesn't always behave the same way. On
         * some systems it was letting c<32 go through, causing an assert in
         * ascii.c. */
        return ((c >= 32)
                && (c < 127)
                && (c != '%')   /* printf special char */
                &&(c != '^')    /* ascii.c special char */
            );
}

int conv_get_word(char *instr, char **beg, char **end)
{
        char *inp = instr;

        while (*inp && !isalpha(*inp)) {
                inp++;
        }

        if (!*inp) {
                return 0;
        }

        *beg = inp;

        while (*inp && isalpha(*inp)) {
                inp++;
        }

        *end = inp;

        return 1;
}

int conv_is_keyword(struct conv *conv, char *word)
{
        int index;

        if (!conv_keyword_highlighting) {
                return 0;
        }

        index = conv_lookup_keyword(conv, word);
        if (index == -1) {
                return 0;
        }
        return CONV_IS_KEYWORD | (bitset_tst(conv->marked, index) ?
                                  CONV_IS_MARKED : 0);
}

int conv_init(void)
{
        char *val = cfg_get("keyword-highlighting");
        if (!val || strcasecmp(val, "yes")) {
                conv_keyword_highlighting = 0;
        }
        return 0;
}

void conv_enable_keyword_highlighting(int enable)
{
        conv_keyword_highlighting = ! !enable;
}
