#ifndef __WORKINGENTRY_H__
#define __WORKINGENTRY_H__

#include <stdlib.h>

#define MAX_NAME_LEN                    100

typedef struct {
        int     fd;
        char    name[MAX_NAME_LEN];
        int     *worker;
        int     real_fd;
        int     userID;        
} WorkingEntry;


WorkingEntry *we_new(int fd, char *name, int *worker, int real_fd, int userID);

int we_get_userid(WorkingEntry *entry);

void we_destroy(WorkingEntry *entry);



#endif
