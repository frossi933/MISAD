#include <stdlib.h>
#include <string.h>

#include "WorkingTable.h"

#define ALLOCATED_ENTRIES       20


WorkingTable *wt_new(){

	WorkingTable *ret = (WorkingTable *)malloc(sizeof(WorkingTable));
	ret->entries = (WorkingEntry **)malloc(sizeof(WorkingEntry *) * ALLOCATED_ENTRIES);
	ret->n_entries = 0;
	ret->allocated = ALLOCATED_ENTRIES;
	return ret;
}


WorkingTable *wt_add_entry(WorkingTable *table, WorkingEntry *entry){

	if(table->n_entries == table->allocated){
		/* Re-size */
		table->entries = (WorkingEntry **)realloc(table->entries, table->allocated + ALLOCATED_ENTRIES);
		table->allocated+=ALLOCATED_ENTRIES;
	}
	
	memcpy(table->entries + table->n_entries, &entry, sizeof(WorkingEntry *));
	table->n_entries++;
	return table;
}

WorkingTable *wt_rm_entry_by_fd(WorkingTable *table, int fd){

	int i, j;
	for(i = 0; i < table->n_entries; i++){
		if( (table->entries[i])->fd == fd ){
			we_destroy(table->entries[i]);
                        table->n_entries--;
			for(j = i; j < table->n_entries; j++)
				memcpy(table->entries + j, table->entries + (j+1), sizeof(WorkingEntry *));
                        break;
		}
	}
	return table;
}

int wt_get_entry_for_user(WorkingTable *table, int cli_id, WorkingEntry **entries){

        int i, j=0, n=0;
    	for(i = 0; i < table->n_entries; i++)
    	    	if( (table->entries[i])->userID == cli_id ){
                    entries[j++] = table->entries[i];
                    n++;
                }

        return n;
}

WorkingEntry *wt_select_by_fd(WorkingTable *table, int fd){

	int i, j;
	for(i = 0; i < table->n_entries; i++){
		if( (table->entries[i])->fd == fd )
                        return table->entries[i];
        }
        return NULL;
}

WorkingEntry *wt_select_by_name(WorkingTable *table, char *name){

	int i, j;
	for(i = 0; i < table->n_entries; i++){
		if( !strcmp((table->entries[i])->name, name) )
                        return table->entries[i];
        }
        return NULL;
}

void wt_destroy(WorkingTable *table){
        
      	int i;
	for(i = 0; i < table->n_entries; i++)
		we_destroy(table->entries[i]);
	
	free(table);
}
