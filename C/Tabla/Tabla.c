#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "Tabla.h"

#define	 ALLOCATED_FILES		20


FileTable 	*ft_new(){

	FileTable *ret = (FileTable *)malloc(sizeof(FileTable));
	ret->files = (Files **)malloc(sizeof(Files *) * ALLOCATED_FILES);
	ret->n_files = 0;
	ret->allocated = ALLOCATED_FILES;
	return ret;
}

FileTable 	*ft_add(FileTable *table, Files *file){

	if(table->n_files == table->allocated){
		/* Re-size */
		table->files = (Files **)realloc(table->files, table->allocated + ALLOCATED_FILES);
		table->allocated+=ALLOCATED_FILES;
	}
	
	memcpy(table->files + table->n_files, &file, sizeof(Files *));
	table->n_files++;
	return table;
}


FileTable 	*ft_rm(FileTable *table, char *file_name){

	int i, j;
	for(i = 0; i < table->n_files; i++){
		if(!strcmp(f_name(table->files[i]), file_name)){
			free(table->files[i]);
        		table->n_files--;
			for(j = i; j < table->n_files; j++)
				memcpy(table->files + j, table->files + (j+1), sizeof(Files *));
			break;
		}
	}
	return table;
}


void	ft_foreach(FileTable *table, VisitorFunc visit, void *extra_data){

	int i;
	for(i=0;i<table->n_files;i++)
		visit(table->files[i], extra_data);
}


Files 	*ft_get_file_by_name(FileTable *table, char *name){

	int i;
	for(i = 0; i < table->n_files; i++){
		if(!strcmp(f_name(table->files[i]), name))
			return table->files[i];
	}
	/* No existe el archivo */
	return NULL;
}


int     ft_get_nfiles(FileTable *table){

	return table->n_files;
}


void    ft_destroy(FileTable *table){

	int i;
	for(i = 0; i < table->n_files; i++)
		file_destroy(table->files[i]);
	free(table);	
}
