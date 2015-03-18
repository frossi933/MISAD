#include <stdlib.h>
#include <string.h>

#include "WorkingEntry.h"

WorkingEntry *we_new(int fd, char *name, int *worker, int real_fd, int userID){

        WorkingEntry *ret = (WorkingEntry *)malloc(sizeof(WorkingEntry));
        ret->fd = fd;
        strcpy(ret->name, name);
        ret->worker = worker;
        ret->real_fd = real_fd;
        ret->userID = userID;
        return ret;
}

void we_destroy(WorkingEntry *entry){
        
        free(entry);
}

