#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>       
#include <sys/types.h>       
#include <arpa/inet.h>      
#include <unistd.h>        
#include <string.h>  
#include <errno.h>  
#include <pthread.h>

#include "Servidor.h"
#include "Socket.h"

#define combine(c, w)              ((c)<<16 | (w))
#define divide_c(n)                (((n) & 0xffff0000)>>16)
#define divide_w(n)                ((n) & 0x0000ffff)


void *worker_to_client(void *arg){

        int client = divide_c((int)arg);
        int work_conn = divide_w((int)arg);

 	while(1){	

		char buffer[BUFF_SIZE] = " ";
		int nread;
		nread = read(work_conn, buffer, BUFF_SIZE);

		if(!strncmp(buffer, "BYE", 3)){
			close(work_conn);
			return NULL;
		}

		if(nread < 0 && errno == EBADF){
                        fprintf(stderr, "Error reading from worker, worker disconnected...\n");
                        write(client, "ERROR WORKER DISCONNECTED\n", 26);
                        close(client);
                        return NULL;
                }

		write(client, buffer, strlen(buffer));
	}
}

void *client_to_worker(void *arg){

        int client = divide_c((int)arg);
        int work_conn = divide_w((int)arg);

 	while(1){	

		char buffer[BUFF_SIZE] = " ";
		int nread;
		nread = read(client, buffer, BUFF_SIZE);
		if(!strncmp(buffer, "BYE", 3)){
                        close(client);
                        write(work_conn, "BYE\n", 4);
        		return NULL;
                }
		if(nread < 0 && errno == EBADF){
			fprintf(stderr, "Error reading from client, client disconnected...\n");
                        write(work_conn, "BYE\n", 4);
                        return NULL;
                }

		write(work_conn, buffer, strlen(buffer));
	}
}

void *socket_process(void *arg){

	char *wrk_ip = ((Tools *)arg)->wrk_ip;
	int client = ((Tools *)arg)->client;
	int work_conn;
	
	srand(time(NULL));

	struct sockaddr_in servaddr;
	work_conn = socket(AF_INET, SOCK_STREAM, 0);
	memset(&servaddr, 0, sizeof(servaddr));
	servaddr.sin_family      = AF_INET;
	inet_aton(wrk_ip, &servaddr.sin_addr);		       // transforma la dir addr de string al formato binario de las dir IP
	servaddr.sin_port        = htons(rand()%WRK_N + 8020); // deberiamos usar WC_PORT, pero para realizar las pruebas usamos este codigo

	if (connect(work_conn, (struct sockaddr *)&servaddr, sizeof(servaddr) ) < 0){
		fprintf(stderr, "Error connecting worker\n");
                write(client, "ERROR CONNECTING WORKER\n", 24);
                close(client);
		free(arg);
                return NULL;
        }

        pthread_t ctow, wtoc;
        pthread_create(&ctow, NULL, client_to_worker, combine(client, work_conn));
        pthread_create(&wtoc, NULL, worker_to_client, combine(client, work_conn));
	free(arg);

        return NULL;
}
