#ifndef __TABLA_H__
#define __TABLA_H__

#include <stdlib.h>

#include "../File/File.h"

typedef struct {
	Files	**files;
	int	n_files;
	int	allocated;
} FileTable;

typedef void (*VisitorFunc) (void *data, void *extra_data);


FileTable       *ft_new();

FileTable       *ft_add(FileTable *table, Files *file);

FileTable       *ft_rm(FileTable *table, char *name_file);

void            ft_foreach(FileTable *table, VisitorFunc visit, void *extra_data);

Files           *ft_get_file_by_name(FileTable *table, char *name);

int             ft_get_nfiles(FileTable *table);

void            ft_destroy(FileTable *table);

#endif
