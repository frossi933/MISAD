#ifndef __SERVIDOR_H__
#define __SERVIDOR_H__


#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

#include "SList/SList.h"
#include "Socket.h"

#define PASSWORD                        1123456789
#define WRK_N				3
#define LOCALHOST			"127.0.0.1"
#define CS_PORT				8000				// puerto de comunicacion entre Client-Server
#define WS_PORT				8001				// puerto de comunicacion entre Worker-Server
#define WC_PORT                         8000                            // puerto de comunicacion entre Worker-Client
#define BUFF_SIZE			1024

pthread_t 			listener;
pthread_mutex_t 	        sem_client_list;
pthread_mutex_t                 sem_wrk_list;
pthread_cond_t                  new_wrk_cond;
pthread_cond_t                  new_client_cond;

typedef struct {
        int     conn;
        SList   **list;
} ConnList;

typedef struct {
	char		*wrk_ip;
	int		client;
} Tools;

#endif
