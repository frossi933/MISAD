#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>
#include <pthread.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <signal.h>

#include "Servidor.h"
#include "Dispatcher.h"

pthread_mutex_t sem_wrk_list = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t sem_client_list = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t new_wrk_cond = PTHREAD_COND_INITIALIZER;
pthread_cond_t new_client_cond = PTHREAD_COND_INITIALIZER;
int worker_conns[WRK_N];
char *worker_ips[WRK_N];

void *get_worker_ip(void *arg){

	int worker = (int)arg;
        int pass;
	char buffer[BUFF_SIZE];
	read(worker, buffer, BUFF_SIZE);		// (CON myIP Pass), Password = 10 digitos
	
	if(!strncmp(buffer, "CON ", 4)){
                char ip_str[16];                        // 16 long max de una IPv4
                if(sscanf(buffer+4, "%s %d", ip_str, &pass) < 2)
                        return NULL;

                if (pass != PASSWORD){
                        write(worker, "INCORRECT PASSWORD\n", 19);
                        close(worker);
                        return NULL;
                }

                struct in_addr aux;
		if(inet_aton(ip_str, &aux) == 0){        // vemos si es valida la direccion ip
			write(worker, "COMMAND ERROR\n", 14);
			close(worker);
			return NULL;
		} else {
			char *ret = (char *)malloc(sizeof(char) * BUFF_SIZE);
			strcpy(ret, ip_str);
			return ret;
		}
	} else {
		write(worker, "COMMAND ERROR\n", 14);
		close(worker);
                return NULL;
	}
}

static int load_workers(int n_workers, int wrk_conn){

	int i = 0;
	pthread_t *t = (pthread_t *)malloc(sizeof(pthread_t) * n_workers);
	char reply[BUFF_SIZE];

	while(i < n_workers){
		if ( (worker_conns[i] = accept(wrk_conn, NULL, NULL) ) < 0)
			continue;
		pthread_create(&t[i], NULL, get_worker_ip, worker_conns[i]);
		i++;
	}

	/* Obtengo el resultado de la funcion "get_worker_ip" */
	for(i = 0; i < n_workers; i++){
		pthread_join(t[i], &worker_ips[i]);
                if(worker_ips[i] == NULL){
			/* si se produjo algun error, liberamos memoria y salimos */
                        int j;
                        free(t);
                        for(j = i-1; j >= 0; j--)
                                free(worker_ips[j]);
                        return -1;
                }
        }
	free(t);

	/* Le envio a cada Worker su ID, el numero de workers con los que tiene que conectarse y dichas las ip's */
	for(i = 0; i < n_workers; i++){
		char to_send[BUFF_SIZE] = " ";
		int j;

		sprintf(to_send, "OK %d %d ", i, n_workers - 1);
		for(j = 0; j < n_workers; j++){
			strcat(to_send, worker_ips[j]);
                        strcat(to_send, " ");
                }
		to_send[strlen(to_send)] = '\n';
		write(worker_conns[i], to_send, strlen(to_send));
	}

	/* Espero la confirmacion de cada worker */
	for(i = 0; i < n_workers; i++){
		read(worker_conns[i], reply, 2);
		if(strncmp(reply, "OK", 2)){
			/* si se produjo algun error, liberamos memoria y salimos */
                        for(i = 0; i < n_workers; i++)
                                free(worker_ips[i]);
                        return -1;
                }
	}
	
	return 0;
}


void *listen_wrk(void *arg){

        SList **list = ((ConnList *)arg)->list;
        int     conn = ((ConnList *)arg)->conn;
        char msg[BUFF_SIZE] = " ";

        while(strcmp(msg, "LOCK\n"))
                read(conn, msg, BUFF_SIZE);

        pthread_mutex_lock(&sem_wrk_list);
        *list = slist_append(*list, conn);
        pthread_mutex_unlock(&sem_wrk_list);
        pthread_cond_signal(&new_wrk_cond);

        return NULL;
}


void *sync_workers(void *arg){

        int n_wrks = (int) arg;
        int i;

        ConnList *wrkList = (ConnList *)malloc(sizeof(ConnList) * n_wrks);
        SList **aux = (SList **)malloc(sizeof(SList *));
        *(aux) = NULL;

        pthread_t *listen_t = (pthread_t *)malloc(sizeof(pthread_t) * n_wrks);
        for(i = 0; i < n_wrks; i++){
                wrkList[i].list = aux;
                wrkList[i].conn = worker_conns[i];
                pthread_create(&listen_t[i], NULL, listen_wrk, &wrkList[i]);                    ///
        }

	while(1){
                int *new_wrk = NULL;
                char msg[BUFF_SIZE]=" ";

		pthread_mutex_lock(&sem_wrk_list);
		while((new_wrk = slist_get_elem(wrkList->list)) == NULL)
			/* Esperamos a que algun worker solicite un lock */
                        pthread_cond_wait(&new_wrk_cond, &sem_wrk_list);
		pthread_mutex_unlock(&sem_wrk_list);

                write(*new_wrk, "OK\n", 3);
                while(strcmp(msg, "UNLOCK\n"))
                        read(*new_wrk, msg, BUFF_SIZE);

                pthread_t new_listen_t;
                for(i = 0; i < n_wrks; i++){
                        if(*new_wrk == wrkList[i].conn){
                                pthread_create(&new_listen_t, NULL, listen_wrk, &wrkList[i]);
                                break;
                        }
                }

                free(new_wrk);
        }
}

int main(int argc, char **argv){

	int wrk_conn;

	srand(time(NULL));

	/* Abrimos el puerto donde nos comunicaremos con los workers */
	if((wrk_conn = open_port(WS_PORT)) < 0){
		fprintf(stderr, "Error opening port\n");
		return 1;
	}

	/* Cargamos los workers */
	if(load_workers(WRK_N, wrk_conn) < 0){
		fprintf(stderr, "Error loading workers\n");
		return 1;
	}

        printf("Workers loaded...\n");

        /* Creamos el thread que se encarga de sincronizar las creaciones de archivos de los workers */
        pthread_t sync_t;
        pthread_create(&sync_t, NULL, sync_workers, WRK_N);

	/* Creamos la lista en donde iremos colocando los nuevos clientes que se conecten al servidor */
        SList **clientList = (SList **)malloc(sizeof(SList *));
        *clientList = NULL;

	/* antes de aceptar clientes, nos aseguramos que si alguno se cae y se produce una señal SIGPIPE, el servidor la ignore */
	signal(SIGPIPE, SIG_IGN);

	/* Comenzamos a atender clientes */
	pthread_create(&listener, NULL, listenTCP, clientList);
	while(1){
                int *new_client = NULL;
		pthread_t socket_proc;
		Tools *newClientTools = (Tools *)malloc(sizeof(Tools));

		pthread_mutex_lock(&sem_client_list);
		while((new_client = slist_get_elem(clientList)) == NULL)
			/* Esperamos a que haya algun cliente conectado */
                        pthread_cond_wait(&new_client_cond, &sem_client_list);
		pthread_mutex_unlock(&sem_client_list);

		newClientTools->wrk_ip = worker_ips[rand()%WRK_N];              // asigno aleatoriamente el worker
		newClientTools->client = *new_client;
                free(new_client);

		/* Lanzamos el proceso socket que atendera este nuevo cliente */		
		pthread_create(&socket_proc, NULL, socket_process, newClientTools);
	}
}
