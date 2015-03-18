#ifndef __WORKER_H__
#define __WORKER_H__

#include "../Tabla/Tabla.h"
#include "WorkingTable.h"

#define PASSWORD                        1123456789

FileTable *localFT;
WorkingTable *localWT;
int fd_ref = 1;                  // referencia para seleccionar el FD de los archivos abiertos
int serv_conn;
int my_id;

void *worker(void *arg);

#endif
