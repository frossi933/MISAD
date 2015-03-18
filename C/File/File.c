#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "File.h"

#define BUFF_SIZE                                            1024
#define min(n, m)                                            (((n)<=(m))?(n):(m))


Files	*file_create(char *name){

	Files *ret = (Files *)malloc(sizeof(Files));

	strncpy(f_name(ret), name, MAX_NAME_LEN);
	f_data(ret) = NULL;
        f_offset(ret) = 0;
	return ret;
}


void 	file_read(Files *file, int size, char *buffout){

        int min_read;
	if(file == NULL || f_data(file) == NULL || (min_read = min(size, strlen(f_data(file)) - f_offset(file))) == 0)
                strcpy(buffout, " ");
	else {
		strncpy(buffout, f_data(file) + f_offset(file), min_read);
                f_offset(file) += min_read;
        }
}


int	file_write(Files *file, char *new_data, int size){

	if(file == NULL)
                return -1;

	int written;
        int len_new_data;

        /* elimino los caracteres '\n' y '\r' */
        len_new_data = strlen(new_data);
        if(new_data[strlen(new_data) - 1] == '\n')
                len_new_data--;
        if(new_data[len_new_data - 1] == '\r')
                len_new_data--;

        if(f_data(file) == NULL){
        	char *result = (char *)malloc(sizeof(char) * min(size, len_new_data));
                strncpy(result, new_data, min(size, len_new_data));
              	f_data(file) = result;

        } else {

                char aux[BUFF_SIZE];
                int total_size = min(size, len_new_data) + strlen(f_data(file));
                f_data(file) = (char *)realloc(f_data(file), total_size);

                if(f_data(file) == NULL)
                        return -1;

                strncat(f_data(file), new_data, min(size, len_new_data));
        }

	return 0;
}


void    file_restart_offset(Files *file){
        f_offset(file) = 0;
}


void	file_destroy(Files *file){

	free(f_name(file));
	if(f_data(file) != NULL)
		free(f_data(file));
	free(file);
}
