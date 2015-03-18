#ifndef __SLIST_H__
#define __SLIST_H__

#include <stdlib.h>
#include <stddef.h>


typedef struct _SList {
        void   *data;
        struct _SList *next;
} SList;

#define slist_data(l)       (l)->data
#define slist_next(l)       (l)->next
#define slist_is_empty(l)   ((l) == NULL)


SList *slist_append (SList *list, void *data);

int *slist_get_elem (SList **list);

void  slist_destroy (SList *list);


#endif /* __SLIST_H__ */
