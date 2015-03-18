#ifndef __DISPATCHER_H__
#define __DISPATCHER_H__


#include <stdlib.h>
#include <pthread.h> 

#include "SList/SList.h"


#define BUFF_SIZE						1024
#define MAX_CLIENT						10

int id;
pthread_mutex_t sem;

/* Abre el puerto "port" y devuelve el entero que representa la conexion o -1 si hubo un error */
int open_port(int port);

void *connect_client(void *arg);

void *listenTCP(void *_serv);

#endif
