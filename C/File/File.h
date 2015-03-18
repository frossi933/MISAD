#ifndef __FILE_H__
#define __FILE_H__

#include <stdlib.h>


#define f_data(f)                       ((f)->data)
#define f_name(f)                       ((f)->name)
#define f_offset(f)                     ((f)->offset)
#define MAX_NAME_LEN                    100	


typedef struct {
	char    *data;
	char    name[MAX_NAME_LEN];
        int     offset;
} Files;

Files	*file_create(char *name);

void 	file_read(Files *file, int size, char *buffout);

int	file_write(Files *file, char *new_data, int size);

void    file_restart_offset(Files *file);

void	file_destroy(Files *file);


#endif
