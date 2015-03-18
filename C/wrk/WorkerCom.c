#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>       
#include <sys/types.h>       
#include <arpa/inet.h> 
#include <errno.h>     

#include "WorkerCom.h"


int open_port(int port){

	int serv, tr = 1;
	struct sockaddr_in servaddr;
	serv = socket(AF_INET, SOCK_STREAM, 0);

	servaddr.sin_family      = AF_INET;
	servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
	servaddr.sin_port        = htons(port);

	if (setsockopt(serv, SOL_SOCKET, SO_REUSEADDR, &tr, sizeof(int)) == -1) {
            	fprintf(stderr, "Error setting socket options\n");
		perror("setsockopt:");
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

int connect_to(char *ip, int port){

	struct sockaddr_in servaddr;
	int conn = socket(AF_INET, SOCK_STREAM, 0);

	memset(&servaddr, 0, sizeof(servaddr));
	servaddr.sin_family = AF_INET;
	inet_aton(ip, &servaddr.sin_addr);			// (in network bite order)
	servaddr.sin_port = htons(port); 			// converts into network bite order(Host TO Network Short)

	if (connect(conn, (struct sockaddr *)&servaddr, sizeof(servaddr) ) < 0){
                fprintf(stderr, "Error calling connect()\n");
		perror("connect:");
	        return -1;
        }
	return conn;
}

int accept_conn(int port_conn){

	int aux;
	while((aux = accept(port_conn, NULL, NULL)) < 0);
	return aux;	
}

void broadcast(char *msg, int size){

	int i;
	for(i = 0; i < wrks_total; i++)
		write(connected[i], msg, size);
}

int read_worker_reply(int conn, char *buffout){

        int i;
        while(!(i = read(conn, buffout, BUFF_SIZE)));
        return i;
}
