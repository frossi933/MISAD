#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>       
#include <sys/types.h>       
#include <arpa/inet.h>      
#include <unistd.h>        
#include <string.h>    
#include <pthread.h>

#include "Dispatcher.h"
#include "Servidor.h"
#include "SList/SList.h"

SList *clientList;
extern pthread_mutex_t sem_list;
pthread_mutex_t sem = PTHREAD_MUTEX_INITIALIZER;
int id = -1;


int open_port(int port){

	int serv, tr = 1;
	struct sockaddr_in servaddr;
	serv = socket(AF_INET, SOCK_STREAM, 0);

	servaddr.sin_family      = AF_INET;
	servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
	servaddr.sin_port        = htons(port);

	if (setsockopt(serv, SOL_SOCKET, SO_REUSEADDR, &tr, sizeof(int)) == -1) {
            	fprintf(stderr, "Error setting socket options\n");
		perror("setsockopt");
            	return -1;
	}

	if(bind(serv, (struct sockaddr *) &servaddr, sizeof(servaddr)) < 0 ){
		fprintf(stderr, "Error calling bind() %d\n", port);
		perror("bind:");
		return -1;
	}

	if(listen(serv, MAX_CLIENT) < 0 ){
		fprintf(stderr, "Error calling listen()\n");
		perror("listen:");
		return -1;                         
	}

	return serv;
}


void *connect_client(void *arg){

        int conn_c = (*(ConnList **)arg)->conn;
        SList **list = (*(ConnList **)arg)->list;

	char buffer[BUFF_SIZE];
	int my_id;

	read(conn_c, buffer, BUFF_SIZE);

	if(!strncmp(buffer, "CON", 3)){

	        pthread_mutex_lock(&sem);
	        /* Region critica */
	        my_id = ++id;                   // ID del nuevo cliente
	        /* Fin Region critica */
	        pthread_mutex_unlock(&sem);

        	fprintf(stderr, "New client %d connected\n", my_id);
		sprintf(buffer,"OK ID %d\n", my_id);
		write(conn_c, buffer, strlen(buffer));

		/* Agrego el cliente a la lista (protegiendolo con el semaforo sem_client_list) */
		pthread_mutex_lock(&sem_client_list);
		*list = slist_append(*list, conn_c);
		pthread_mutex_unlock(&sem_client_list);
                pthread_cond_signal(&new_client_cond);
	} else {
		write(conn_c, "COMMAND ERROR\0", 14);
		close(conn_c);
	}

	return NULL;
}


void *listenTCP(void *arg){

        ConnList *l = (ConnList *)malloc(sizeof(ConnList));
        l->list = (SList **)arg;
	int client, serv;

	if((serv = open_port(CS_PORT)) < 0){
		fprintf(stderr, "Error opening Client-Server port\n");
		return NULL;
	}

	while (1) {
                pthread_t t;

		if ( (client = accept(serv, NULL, NULL) ) < 0 )
			continue;

                l->conn = client;
		pthread_create(&t, NULL, connect_client, &l);
	}
}
