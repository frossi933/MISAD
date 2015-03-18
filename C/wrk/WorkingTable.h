#ifndef __WORKINGTABLE_H__
#define __WORKINGTABLE_H__

#include <stdlib.h>

#include "WorkingEntry.h"

typedef struct {
        WorkingEntry    **entries;
        int             n_entries;
        int             allocated;
} WorkingTable;


WorkingTable *wt_new();

WorkingTable *wt_add_entry(WorkingTable *table, WorkingEntry *entry);

int           wt_get_entry_for_user(WorkingTable *table, int cli_id, WorkingEntry **entries);

WorkingTable *wt_rm_entry_by_fd(WorkingTable *table, int fd);

WorkingEntry *wt_select_by_fd(WorkingTable *table, int fd);

WorkingEntry *wt_select_by_name(WorkingTable *table, char *name);

void          wt_destroy(WorkingTable *table);

#endif
