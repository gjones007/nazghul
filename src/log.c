#include "log.h"
#include "list.h"
#include "common.h"
#include "console.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#define LOG_ENTRY_BUF_SZ 128

static struct list log_q;
static struct list log_stk;
static int log_group;

struct log_entry {
        struct list q_hook;
        struct list stk_hook;
        char buf[LOG_ENTRY_BUF_SZ];
        char *ptr;
        int room;
};

struct log_entry *log_entry_new()
{
        struct log_entry *entry = (struct log_entry*)calloc(1, sizeof(*entry));
        assert(entry);
        entry->ptr = entry->buf;
        entry->room = sizeof(entry->buf);
        return entry;
}

static void log_entry_del(struct log_entry *entry)
{
        free(entry);
}

static void log_entry_print(struct log_entry *entry, char *fmt, va_list args)
{
        int wrote = vsnprintf(entry->ptr, entry->room, fmt, args);
        if (wrote > 0) {
                entry->room -= wrote;
                entry->ptr += wrote;
        } else {
                entry->room = 0;
        }
}

static inline void log_print_queued_msgs()
{
        struct list *elem;

        elem = log_q.next;

        while (elem != &log_q) {

                struct log_entry *entry;
                
                entry = outcast(elem, struct log_entry, q_hook);
                elem = elem->next;
                consolePrint("%s\n", entry->buf);
                list_remove(&entry->q_hook);
                log_entry_del(entry);
        }
}

static inline void log_push(struct log_entry *entry)
{
        list_add_tail(&log_q, &entry->q_hook);
        list_add(&log_stk, &entry->stk_hook);
        log_begin_group();
}

static inline void log_pop()
{
        assert(! list_empty(&log_stk));
        
        /* Pop the topmost entry from the msg stack */
        list_remove(log_stk.next);
        
        /* If the msg stack is now empty then log all msgs on the queue */
        if (list_empty(&log_stk))
                log_print_queued_msgs();
        log_end_group();
}

void log_init(void)
{
        list_init(&log_q);
        list_init(&log_stk);
        log_group = 0;
}

void log_begin(char *fmt, ...)
{
        va_list args;

        struct log_entry *entry = log_entry_new();
        log_push(entry);
        va_start(args, fmt);
        log_entry_print(entry, fmt, args);
        va_end(args);
}

void log_continue(char *fmt, ...)
{
        va_list args;
        struct log_entry *entry;

        assert(! list_empty(&log_stk));
        entry = outcast(log_stk.next, struct log_entry, stk_hook);
        va_start(args, fmt);
        log_entry_print(entry, fmt, args);
        va_end(args);
}

void log_end(char *fmt, ...)
{
        va_list args;
        struct log_entry *entry;

        assert(! list_empty(&log_stk));
        entry = outcast(log_stk.next, struct log_entry, stk_hook);
        va_start(args, fmt);
        log_entry_print(entry, fmt, args);
        va_end(args);
        log_pop();
}

void log_msg(char *fmt, ...)
{
        va_list args;
        struct log_entry *entry = log_entry_new();

        log_push(entry);
        va_start(args, fmt);
        log_entry_print(entry, fmt, args);
        va_end(args);
        log_pop();        
}

void log_begin_group()
{
        log_group++;
}

void log_end_group()
{
        log_group--;
        if (log_group == 0)
                consolePrint("\n");
}
