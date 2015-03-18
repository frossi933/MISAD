/********************** PROTOCOLO DE COMUNICACION ENTRE LOS WORKERS ****************


                envio                                        recibo                                         Descripcion
                -----                                        ------                                         -----------

-->        "WRK lsd"                                  Arch1 Arch2 ... ArchN               Lista con todos los archivos del FS local del worker

-->        "WRK opn" ID NAME                            "ok" FD                           Apertura correcta, FD es su descriptor de archivo
                                                        "e1"                              Error 1: el archivo no existe
                                                        "e2"                              Error 2: el archivo ya esta abierto por otra persona

-->        "WRK clo" FD                                 "ok"                              Cerrado correctamente

-->        "WRK wrt" FD SIZE BUFF                       "ok"                              Escritura correcta
                                                        "e3"                              Error 3: Fallo durante la escritura

-->        "WRK rea" FD SIZE                            BUFF'\n'                          BUFF es el resultado de la lectura

-->        "WRK del" NAME                               "ok"                              Borrado correctamente.
                                                        "e1"                              Error 1: el archivo no existe
                                                        "e2"                              Error 2: el archivo esta abierto

-->        "WRK exs" NAME                               "y"                               Existe el archivo NAME
                                                        "n"                               NO existe el archivo NAME
                                                                                
************************************************************************************/

#ifndef __WORKERCOM_H__
#define __WORKERCOM_H__

#define         WS_PORT                                  8003                // puerto de comunicacion Worker-Server
#define         MAX_CLIENT                               10
#define         N_MES_WRK                                7                   // numero de mensajes entre workers
#define         MES_WRK_LEN                              3                   // tamaño de mensaje entre workers
#define         WW_PORT                                  8002                // puerto de comunicacion entre workers
#define         BUFF_SIZE                                1024

#include "../Tabla/Tabla.h"

int wrks_total;
int *connected;
int *accepted;


/* Abre el puerto "port" y devuelve el entero que representa la conexion o -1 si hubo un error */
int open_port(int port);

/* Se conecta con la direccion "ip" en el puerto "port". Devuelve -1 si hubo un error o el descriptor de la conexion si no hubo problemas */
int connect_to(char *ip, int port);

/* Acepta una conexion y devuelve dicho descriptor o -1 si hubo error */
int accept_conn();

/* Envia "msg" de tamaño "size" a todos los workers distintos del que lo llamo */
void broadcast(char *msg, int size);

/* Obtiene la respuesta del worker en la coneccion "conn" y lo guarda en "buffout". Devuelve -1 si hubo error */
int read_worker_reply(int conn, char *buffout);

#endif
