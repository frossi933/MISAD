#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/socket.h>       
#include <sys/types.h>       
#include <arpa/inet.h>      
#include <unistd.h>
#include <errno.h>
#include <pthread.h>      


#include "Worker.h"
#include "WorkerCom.h"
#include "WorkingEntry.h"

#define LOCALHOST                                            "127.0.0.1"
#define BUFF_SIZE                                            1024
#define COMM_LEN                                             3
#define N_COMM                                               8
//#define WW_PORT                                            8001          // puerto de comunicacion entre workers
#define ENOOPN                                               3             // error agregado por mi, para archivos que no se encuentran abiertos
#define set_client_id(n)                                     ((n) = ((n) | my_id<<24))
                        
/*                                                           ----------------------------------------------------
      CLIENT_ID:                                             |    ID_WRK    |           ID_INT_CLIENT           |
                                                             ----------------------------------------------------
                                                            32              24                                  0 
*/


char commands[N_COMM][COMM_LEN] = { "LSD",
                                    "DEL",
                                    "CRE",
                                    "OPN",
                                    "WRT",
                                    "REA",
                                    "CLO",
                                    "BYE"
                                  };

char mes_wrk[N_MES_WRK][MES_WRK_LEN] = { "lsd",
                                         "opn",
                                         "wrt",
                                         "rea",
                                         "clo",
                                         "del",
                                         "exs"
                                       };

FileTable *localFT;
int id_ref;
pthread_mutex_t sem_fd = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t sem_id = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t sem_localFT = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t sem_localWT = PTHREAD_MUTEX_INITIALIZER;

static void list_files(void *_file, void *extra){

        Files *file = (Files *)_file;
        char *buffout = (char *)extra;
        strcat(buffout, " ");
        strcat(buffout, f_name(file));
}


static inline void lock_localWT(void){
        pthread_mutex_lock(&sem_localWT);
}

static inline void lock_localFT(void){
        pthread_mutex_lock(&sem_localFT);
}

static inline void unlock_localWT(void){
        pthread_mutex_unlock(&sem_localWT);
}

static inline void unlock_localFT(void){
        pthread_mutex_unlock(&sem_localFT);
}


static inline void server_lock(void){

        char msg[BUFF_SIZE] = "";

        write(serv_conn, "LOCK\n", 5);
        while(strncmp(msg, "OK", 2))
                read(serv_conn, msg, BUFF_SIZE);
}

static inline void server_unlock(void){

        write(serv_conn, "UNLOCK\n", 7);
}


static void worker_lsd(char *buffout){

        int i;

        broadcast("WRK lsd\n", 8);

        strcpy(buffout, "OK ");
        for(i = 0; i < wrks_total; i++){
                char reply[BUFF_SIZE] = "";
                int n;
                read_worker_reply(connected[i], reply);

                /* Le sacamos el '\n' */
                n = strlen(reply);
                if(reply[n-1] == '\n')
                        n--;
                if(reply[n-1] == '\r')
                        n--;

                strncat(buffout, reply, n);
        }

        /* protegemos el FS local */
        lock_localFT();        
        ft_foreach(localFT, list_files, buffout);
        unlock_localFT();

        strcat(buffout, "\n");
}


static int worker_delete(char *name){

        /* protegemos el FS local */
        lock_localFT();
        Files *to_rm = ft_get_file_by_name(localFT, name);

        if(to_rm == NULL){
                unlock_localFT();
                /* El archivo no esta en el FS local */
                char msg[BUFF_SIZE] = "", aux[BUFF_SIZE] = "";
                int i, ret = ENOENT;

                sprintf(msg, "WRK del %s\n", name);
                broadcast(msg, strlen(msg));

                for(i = 0; i < wrks_total; i++){
                        read_worker_reply(connected[i], aux);
                        if(!strncmp(aux, "ok", 2))
                                /* Fue borrado en la otra tabla */
                                ret = 0;
                        else if (!strncmp(aux, "e2", 2))
                                /* No esta permitido borrar ese archivo */
                                ret = EPERM;
                }
                return ret;
        }

        /* El archivo esta en el FS local */
        /* protegemos el FS local */
        lock_localWT();
        WorkingEntry *to_rem = wt_select_by_name(localWT, name);
        unlock_localWT();

        if(to_rem == NULL){
                /* No esta abierto */
                localFT = ft_rm(localFT, name);
                unlock_localFT();
                return 0;
        } else {
                /* Esta abierto */
                unlock_localFT();
                return EPERM;
        }
}

static int worker_create(char *name){

        /* protegemos el FS global, para evitar dos creaciones simultaneas */
        server_lock();
        /* protegemos el FS local */
        lock_localFT();
        Files *f = ft_get_file_by_name(localFT, name);

        if(f != NULL){
                unlock_localFT();
                server_unlock();
                return EEXIST;
        } else {
                unlock_localFT();

                char msg[BUFF_SIZE] = "";
                char aux[BUFF_SIZE] = "";
                int i, exist = 0;

                sprintf(msg, "WRK exs %s\n", name);
                broadcast(msg, strlen(msg));
                for(i = 0; i < wrks_total; i++){
                        read_worker_reply(connected[i], aux);
                        if(aux[0] == 'y')
                                exist = 1;
                }

                if(exist){
                        server_unlock();
                        return EEXIST;
                }
        }

        /* No existe en ningun FS */
        Files *newFile = file_create(name);

        localFT = ft_add(localFT, newFile);
        unlock_localFT();
        server_unlock();
        return 0;
}

static int        worker_open(char *name, int cli_id, int *fd_ret){

        /* protegemos el FS local */
        lock_localWT();
        WorkingEntry *file_entry = wt_select_by_name(localWT, name);

        if(file_entry == NULL){
                /* No esta abierto */
                lock_localFT();
                Files *file = ft_get_file_by_name(localFT, name);
                unlock_localFT();

                if(file == NULL){
                        /* No se encuentra en la tabla local */
                        unlock_localWT();

                        char msg[BUFF_SIZE] = "", reply[BUFF_SIZE] = "";
                        int i, ret = ENOENT;

                        sprintf(msg, "WRK opn %d %s\n", cli_id, name);
                        broadcast(msg, strlen(msg));
                        for(i = 0; i < wrks_total; i++){
                                read_worker_reply(connected[i], reply);
                                if(!strncmp(reply, "ok", 2)){
                                        pthread_mutex_lock(&sem_fd);
                                        WorkingEntry *new = we_new(fd_ref, name, &connected[i], atoi(reply+3), cli_id);// ENTRADA PARA ARCHIVO EXTERNO
                                        *fd_ret = fd_ref++;
                                        pthread_mutex_unlock(&sem_fd);

                                        lock_localWT();
                                        localWT = wt_add_entry(localWT, new);
                                        unlock_localWT();
                                        ret = 0;
                                } else if(!strncmp(reply, "e2", 2)){
                                        /* Se encuentra abierto por otra persona */
                                        ret = EPERM;
                                }
                        }
                        return ret;
                } else {
                        /* Se encuentra en la tabla local */
                        file_restart_offset(file);
                        pthread_mutex_lock(&sem_fd);
                        file_entry = we_new(fd_ref, name, NULL, 0, cli_id);                                             // ENTRADA PARA ARCHIVO LOCAL
                        *fd_ret = fd_ref++;
                        pthread_mutex_unlock(&sem_fd);
                        localWT = wt_add_entry(localWT, file_entry);
                        unlock_localWT();
                        
                        return 0;
                }
        } else {
                /* Ya esta abierto */
                if(file_entry->userID == cli_id){
                        *fd_ret = file_entry->fd;
                        unlock_localWT();
                        return 0;
                } else {
                        unlock_localWT();
                        return EPERM;
                }
        }
}


static int        worker_write(int fd, int size, char *buffer, int cli_id){

        lock_localWT();
        WorkingEntry *file_entry = wt_select_by_fd(localWT, fd);
        unlock_localWT();        // desbloqueo la tabla, solo este cliente puede modificar esta entrada

        if(file_entry == NULL)
                /* No esta abierto */
                return ENOOPN;
        else {
                if(file_entry->userID == cli_id){
                        if(file_entry->worker == NULL){
                                /* Se enceuntra en el FS local */
                                lock_localFT();
                                Files *file = ft_get_file_by_name(localFT, file_entry->name);
                                if(file_write(file, buffer, size) < 0){
                                        unlock_localFT();
                                        return EAGAIN;
                                } else {
                                        unlock_localFT();
                                        return 0;
                                }
                        } else {
                                /* Se encuentra en otro FS */
                                char msg[BUFF_SIZE] = "", reply[BUFF_SIZE] = "";
                                int i;
                                
                                sprintf(msg, "WRK wrt %d %d %s", file_entry->real_fd, size, buffer);       // Utilizamos el FD real
                                write(*file_entry->worker, msg, strlen(msg));
                                read_worker_reply(*file_entry->worker, reply);
                                if(!strncmp(reply, "ok", 2))
                                        return 0;
                                else
                                        return EAGAIN;
                        }
                } else
                        return EPERM;
        }
}


static int worker_read(int fd, int size, int cli_id, char *buffout){

        /*protegemos el FS local */
        lock_localWT();  
        WorkingEntry *file_entry = wt_select_by_fd(localWT, fd);
        unlock_localWT();        // desbloqueo la tabla, solo este cliente puede modificar esta entrada

        if(file_entry == NULL)
                 /* No esta abierto */
                return ENOOPN;
        else {
                if(file_entry->userID == cli_id){
                        if(file_entry->worker == NULL){
                                /* Se encuentra en el FS local */
                                lock_localFT();
                                Files *file = ft_get_file_by_name(localFT, file_entry->name);
                                file_read(file, size, buffout);
                                unlock_localFT();
                                return 0;
                        } else {
                                /* Se encuentra en otro FS */
                                char msg[BUFF_SIZE] = "", reply[BUFF_SIZE] = "";
                                int i;
                                
                                sprintf(msg, "WRK rea %d %d\n", file_entry->real_fd, size);       // Utilizamos el FD real
                                write(*file_entry->worker, msg, strlen(msg));
                                read_worker_reply(*file_entry->worker, reply);
                                strcpy(buffout, reply);
                                return 0;
                        }
                } else
                        return EPERM;
        }
}


static int worker_close(int fd, int cli_id){

        /* protegemos el FS local */
        lock_localWT();
        WorkingEntry *file_entry = wt_select_by_fd(localWT, fd);
        if(file_entry == NULL){
                /* No esta abierto */
                unlock_localWT();
                return ENOOPN;
        } else {
                if(file_entry->userID == cli_id){
                        if(file_entry->worker == NULL){
                                /* Es un archivo local */
                                localWT = wt_rm_entry_by_fd(localWT, fd);
                                unlock_localWT();
                                return 0;
                        } else {
                                /* Es un archivo externo */
                                unlock_localWT();

                                char msg[BUFF_SIZE] = "", reply[BUFF_SIZE] = "";
                                int i;
                                
                                sprintf(msg, "WRK clo %d\n", file_entry->real_fd);       // Utilizamos el FD real
                                write(*file_entry->worker, msg, strlen(msg));
                                read_worker_reply(*file_entry->worker, reply);
                                lock_localWT();
                                localWT = wt_rm_entry_by_fd(localWT, fd);
                                unlock_localWT();
                                return 0;
                        }
                } else {
                        unlock_localWT();
                        return EPERM;
                }
        }
}


static void worker_bye(int cli_id){
        /* Cerramos todos los archivos abiertos por este usuario */
        int i,n_files;
        WorkingEntry **entries = (WorkingEntry **)malloc(sizeof(WorkingEntry *) * localWT->n_entries);
        n_files = wt_get_entry_for_user(localWT, cli_id, entries);
        for(i = 0; i < n_files; i++)
                worker_close(entries[i]->fd, cli_id);

        free(entries);
}


static void errdesc(int err, char *reply){
        switch(err) {
                case EPERM:         strcpy(reply, "ERROR 1 EPERM: Operacion no permitida\n");
                                                break;
                case ENOENT:        strcpy(reply, "ERROR 2 ENOENT: No existe el archivo\n");
                                                break;
                case ENOOPN:        strcpy(reply, "ERROR 3 ENOOPN: El archivo no esta abierto\n");
                                                break;
                case EEXIST:        strcpy(reply, "ERROR 17 EEXIST: El archivo ya existe\n");
                                                break; 
                case EAGAIN:        strcpy(reply, "ERROR 11 EAGAIN: Fallo la operacion, intente nuevamente\n");               
        }
}

void *worker_handle_worker(void *arg){
        
        int conn = (int)arg;
        char buffer[BUFF_SIZE];
        int i;

        while(1){
                char *ret = (char *) calloc (sizeof(char), BUFF_SIZE);

                while(!read(conn, buffer, BUFF_SIZE));

                if(strncmp(buffer, "WRK ", COMM_LEN + 1)){
                        /* Si existe un error en estas comunicaciones las descarto directamente */
                        fprintf(stderr, "W-W Comunication Error: Incorrect Command");
                        free(ret);
                        continue;
                }

                for(i = 0; i < N_MES_WRK; i++)
                        if(!strncmp(buffer+4, mes_wrk[i], MES_WRK_LEN))
                                break;

                switch(i) {
                        case 0:{/* lsd */
                                lock_localFT();
                                if(ft_get_nfiles(localFT) == 0)
                                        strcpy(ret, "\n");
                                else {
                                        ft_foreach(localFT, list_files, ret);
                                        sprintf(ret, "%s\n", ret+1);            // sumo 1 porque la cadena termina teniendo un " " al principio
                                }
                                unlock_localFT();
                                break;
                        }
                        case 1:{/* opn */
                                char name[MAX_NAME_LEN];
                                int cli_id;
                                Files *file;

                                sscanf(buffer+4, "opn %d %s", &cli_id, name);    // length(cli_id) = 10 cifras
                                lock_localFT();
                                file = ft_get_file_by_name(localFT, name);
                                if(file == NULL)
                                        sprintf(ret, "e1\n");
                                else {
                                        lock_localWT();
                                        WorkingEntry *file_entry = wt_select_by_name(localWT, name);
                                        if(file_entry == NULL){
                                                /* Existe y no esta abierto */
                                                int fd_ret;
                                                file_restart_offset(file);
                                                pthread_mutex_lock(&sem_fd);
                                                file_entry = we_new(fd_ref, name, NULL, 0, cli_id);
                                                fd_ret = fd_ref++;
                                                pthread_mutex_unlock(&sem_fd);
                                                localWT = wt_add_entry(localWT, file_entry);
                                                sprintf(ret, "ok %d\n", fd_ret);
                                        } else {
                                                /* Ya esta abierto */
                                                if(file_entry->userID == cli_id)
                                                        sprintf(ret, "ok %d\n", file_entry->fd);
                                                else
                                                        sprintf(ret, "e2\n");
                                        }
                                        unlock_localWT();
                                }
                                unlock_localFT();
                                break;
                        }
                        case 2:{/* wrt */
                                int fd, size;
                                Files *file;
                                WorkingEntry *file_entry;
                                char buff[BUFF_SIZE], aux[BUFF_SIZE];// TO-DO: hacer los buffer de lectura y escritura mas grandes o de tamaño dinamico.

                                sscanf(buffer + 4, "wrt %d %d ", &fd, &size);
                                lock_localWT();
                                lock_localFT();
                                file_entry = wt_select_by_fd(localWT, fd);
                                file = ft_get_file_by_name(localFT, file_entry->name);

                                sprintf(aux, "wrt %d %d ", fd, size);
                                strncpy(buff, buffer + 4 + strlen(aux), strlen(buffer) - 5 - strlen(aux));   // aqui le eliminamos el ultimo '\n'
                                
                                if( file_write(file, buff, size) < 0 )
                                        sprintf(ret, "e3\n");
                                else
                                        sprintf(ret, "ok\n");
                                unlock_localFT();
                                unlock_localWT();
                                break;
                        }
                        case 3:{/* rea */
                                int fd, size;
                                Files *file;
                                WorkingEntry *file_entry;

                                sscanf(buffer + 4, "rea %d %d", &fd, &size);
                                lock_localWT();
                                lock_localFT();

                                file_entry = wt_select_by_fd(localWT, fd);
                                file = ft_get_file_by_name(localFT, file_entry->name);
                                file_read(file, size, ret);

                                unlock_localWT();
                                unlock_localFT();
                                break;
                        }
                        case 4:{/* clo */
                                int fd;
                                Files *file;
                                WorkingEntry *file_entry;

                                sscanf(buffer + 4, "clo %d", &fd);
                                lock_localWT();
                                localWT = wt_rm_entry_by_fd(localWT, fd);
                                unlock_localWT();
                                sprintf(ret, "ok\n");
                                break;
                        }
                        case 5:{/* del */
                                char name[MAX_NAME_LEN];
                                Files *file;
                                sscanf(buffer + 4, "del %s", name);
                                lock_localFT();
                                file = ft_get_file_by_name(localFT, name);
                                if(file == NULL)
                                        sprintf(ret, "e1\n");
                                else {
                                        lock_localWT();
                                        WorkingEntry *file_entry = wt_select_by_name(localWT, name);
                                        unlock_localWT();
                                        if(file_entry == NULL){
                                                localFT = ft_rm(localFT, name);
                                                sprintf(ret, "ok\n");
                                        } else 
                                                sprintf(ret, "e2\n");
                                }
                                unlock_localFT();
                                break;
                        }
                        case 6:{/* exs */
                                char name[MAX_NAME_LEN];
                                Files *file;
                                sscanf(buffer + 4, "exs %s", name);
                                lock_localFT();
                                file = ft_get_file_by_name(localFT, name);
                                unlock_localFT();
                                if(file == NULL)
                                        sprintf(ret, "n\n");
                                else
                                        sprintf(ret, "y\n");

                                break;
                        }
                        default:{/* error */
                                fprintf(stderr, "W-W Comunication Error: Incorrect Command");
                                continue;
                        }
                }
                /* Le envio la respuesta al pedido */
                write(conn, ret, strlen(ret));
                free(ret);
        }
}

void *worker_handle_client(void *arg){

        int client = *(int *)arg, cli_id;
        int i, j = 0, exit = 0;

        pthread_mutex_lock(&sem_id);
        cli_id = id_ref;
        id_ref++;
        pthread_mutex_unlock(&sem_id);

        /* Obtenemos el id del cliente, el cual es unico en todo el servidor */
        set_client_id(cli_id);

        fprintf(stderr, "Client %d connected to worker\n", cli_id);

        while(!exit){

                char buffer[BUFF_SIZE] = "";
                char *reply = (char *) calloc(sizeof(char), BUFF_SIZE);
                while(!read(client, buffer, BUFF_SIZE));
        
                for(i = 0; i < N_COMM; i++)
                        if(!strncmp(buffer, commands[i], COMM_LEN))
                                break;
                
                switch(i) {
                        case 0:{/* LSD */
                                        if(strlen(buffer)-2 != COMM_LEN)
                                                strcpy(reply, "LSD: COMMAND ERROR\n");
                                        else
                                                worker_lsd(reply);
                                        break;
                                        }
                        case 1:{/* DEL */
                                        char to_delete[MAX_NAME_LEN] = "";
                                        int err;
                                        
                                        if(sscanf(buffer, "DEL %s", to_delete) < 1){
                                                strcpy(reply, "DEL: COMMAND ERROR\n");
                                                break;
                                        }

                                        err = worker_delete(to_delete);
                                        if(err != 0)
                                                errdesc(err, reply);
                                        else
                                                strcpy(reply, "OK\n");
                                        break;
                                        }
                        case 2:{/* CRE */
                                        char to_create[MAX_NAME_LEN] = "";
                                        int err;

                                        if(sscanf(buffer, "CRE %s", to_create) < 1){
                                                strcpy(reply, "CRE: COMMAND ERROR\n");
                                                break;
                                        }

                                        err = worker_create(to_create);

                                        if(err != 0)
                                                errdesc(err, reply);
                                        else
                                                strcpy(reply, "OK\n");
                                        break;
                                        }
                        case 3:{/* OPN */
                                        char to_open[MAX_NAME_LEN] = "";
                                        int err, fd;

                                        if(sscanf(buffer, "OPN %s", to_open) < 1){
                                                strcpy(reply, "OPN: COMMAND ERROR\n");
                                                break;
                                        }

                                        err = worker_open(to_open, cli_id, &fd);

                                        if(err != 0)
                                                errdesc(err, reply);
                                        else
                                                sprintf(reply, "OK %d\n", fd);
                                        break;
                                        }
                        case 4:{/* WRT */
                                        int to_write, size, err;
                                        char buffer2[BUFF_SIZE] = "", aux[BUFF_SIZE] = "";
                                        if(sscanf(buffer, "WRT FD %d SIZE %d", &to_write, &size) < 2){
                                                strcpy(reply, "WRT: COMMAND ERROR\n");
                                                break;
                                        }
                                        sprintf(aux, "WRT FD %d SIZE %d ", to_write, size);
                                        strcpy(buffer2, buffer + strlen(aux));
             
                                        err = worker_write(to_write, size, buffer2, cli_id);
                        
                                        if(err != 0)
                                                errdesc(err, reply);
                                        else
                                                strcpy(reply, "OK\n");
                                        break;
                                        }
                        case 5:{/* REA */
                                        int to_read, size, err;
                                        char *buff = (char *)calloc(sizeof(char), BUFF_SIZE);

                                        if(sscanf(buffer, "REA FD %d SIZE %d", &to_read, &size) < 2){
                                                strcpy(reply, "REA: COMMAND ERROR\n");
                                                break;
                                        }
                                        
                                        err = worker_read(to_read, size, cli_id, buff);
                                        if(err != 0)
                                                errdesc(err, reply);
                                        else
                                                sprintf(reply, "OK %s\n", buff);
                                        free(buff);
                                        break;
                                        }
                        case 6:{/* CLO */
                                        int to_close, err;
                                        if(sscanf(buffer, "CLO FD %d", &to_close) < 1){
                                                strcpy(reply, "CLO: COMMAND ERROR\n");
                                                break;
                                        }

                                        err = worker_close(to_close, cli_id);
                                        if(err != 0)
                                                errdesc(err, reply);
                                        else
                                                strcpy(reply, "OK\n");
                                        break;
                                        }
                        case 7:{/* BYE */
                                        worker_bye(cli_id);
                                        exit = 1;
                                        strcpy(reply, "BYE\n");
                                        break;
                                        }
                        default:{/* ERROR */
                                        strcpy(reply, "COMMAND ERROR\n");
                                        break;
                                        }
                }
                write(client, reply, strlen(reply));
                free(reply);
        }
        close(client);
        fprintf(stderr, "Client %d disconnected from worker\n", cli_id);
        free(arg);
        return NULL;
}


/**************************************************************************************
        CODIGO PARA EJECUTAR EN REALIDAD:
        ---------------------------------     

char *ip_local() {
        struct sockaddr_in host;
        char nombre[255], *ip;
         
        gethostname(nombre, 255);
        host.sin_addr = * (struct in_addr*) gethostbyname(nombre)->h_addr;
        ip = inet_ntoa(host.sin_addr);
        return ip;
}
***************************************************************************************/



/*
         ./worker ServIP ServPort WPort CPort

        Aclaracion: Los dos ultimos argumentos son parte de las pruebas a realizar en una sola PC
*/
int main(int argc, char **argv){

        if(argc != 5){
                fprintf(stderr, "Error Invalid arguments\n");
                return 1;
        }

        int serv_port = atoi(argv[2]);
        char *serv_ip = argv[1], msg[BUFF_SIZE], reply[BUFF_SIZE];
        int client_conn, myconn, new_client;
        int i,j = 0;
        pthread_t *t;

        /* Abrimos el puerto Worker-Worker */
        if((myconn = open_port(atoi(argv[3]))) < 0){
                fprintf(stderr, "Error opening Worker-Worker port\n");
                return 2;
        }

        /* Abrimos el puerto Worker-CLient */
        if((client_conn = open_port(atoi(argv[4]))) < 0){
                fprintf(stderr, "Error opening Worker-Client port\n");
                return 2;
        }

        /* Nos conectamos al servidor */
        if((serv_conn = connect_to(serv_ip, serv_port)) < 0){
                fprintf(stderr, "Error connecting with server\n");
                return 2;
        }
        
        /* Le aviso al servidor que estoy listo y le paso mi IP */
        sprintf(msg,"CON %s %d\n", LOCALHOST, PASSWORD);                    //debemos usar ip_local(), pero para prueba usamos el localhost
        write(serv_conn, msg, strlen(msg));

        /* Espero la respuesta del servidor */
        char ips[BUFF_SIZE];
        read(serv_conn, reply, BUFF_SIZE);
        if(sscanf(reply, "OK %d %d %s", &my_id, &wrks_total, ips) < 3){
                fprintf(stderr, "Error connecting workers with server\n");
                return 3;
        }

        /* Arreglos donde guardamos las conexiones con los workers */
        connected = (int *)malloc(sizeof(int) * wrks_total);
        accepted = (int *)malloc(sizeof(int) * wrks_total);

/**************************************************************************************
        CODIGO PARA EJECUTAR EN REALIDAD:
        ---------------------------------

        char *ip_aux;
        ip_aux = strtok(ips, " ");

        for(i = 0; i < wrks_total; i++){
                if((connected[i] = connect_to(ip_aux, WW_PORT) < 0){
                         fprintf(stderr, "Error connecting others workers\n");
                         return 5;
                }
                ip_aux = strtok(NULL, " ");
        }

***************************************************************************************/

/************************ CODIGO PARA REALIZAR PRUEBAS:        ************************/
        if(atoi(argv[3]) == 8010){
                connected[0] = connect_to(LOCALHOST, 8011);
                connected[1] = connect_to(LOCALHOST, 8012);
        } else if (atoi(argv[3]) == 8011){
                connected[0] = connect_to(LOCALHOST, 8010);
                connected[1] = connect_to(LOCALHOST, 8012);
        } else {
                connected[0] = connect_to(LOCALHOST, 8010);
                connected[1] = connect_to(LOCALHOST, 8011);
        }

/**************************************************************************************/

        /* Acepto las conexiones de los demas workers */
        for(i = 0; i < wrks_total; i++)
                if((accepted[i] = accept_conn(myconn)) < 0){
                        fprintf(stderr, "Error accepting others workers\n");
                        return 4;
                }

        /* Confirmacion al servidor */
        write(serv_conn, "OK\n", 3);

        /* creamos la tabla de archivos y de trabajo*/
        localFT = ft_new();
        localWT = wt_new();

        /* lanzamos los hilos que interactuan con los otros workers */
        pthread_t *whw = (pthread_t *)malloc(sizeof(pthread_t) * wrks_total);
        for(i=0; i<wrks_total; i++)
                pthread_create(&whw[i], NULL, worker_handle_worker, accepted[i]);
        
        while(1){
                pthread_t t;

                if((new_client = accept(client_conn, NULL, NULL)) < 0){
                        fprintf(stderr, "Error connecting new client\n");
                        continue;
                }

                int *client_aux = malloc(sizeof(int));
                *client_aux = new_client;
                pthread_create(&t, NULL, worker_handle_client, client_aux);
        }
}
