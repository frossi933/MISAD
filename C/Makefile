CC=gcc
CFLAGS=-g -lpthread -Wall
SERVOBJS= Dispatcher.o File/File.o Servidor.o SList/SList.o Socket.o Tabla/Tabla.o
WRKOBJS= wrk/Worker.o wrk/WorkerCom.o Tabla/Tabla.o File/File.o wrk/WorkingTable.o wrk/WorkingEntry.o

.SILENT:

all: server worker

server: $(SERVOBJS)
	$(CC) -o $@ $(SERVOBJS) $(CFLAGS)
	
worker: $(WRKOBJS)
	$(CC) -o wrk/$@ $(WRKOBJS) $(CFLAGS)
	
SList.o: SList.c SList.h

File.o: File.c File.h

Tabla.o: Tabla.c Tabla.h ../File/File.o

Dispatcher.o: SList/SList.o Dispatcher.h Servidor.h

Socket.o: Socket.c Socket.h Servidor.h

Servidor.o: Servidor.c SList/SList.o wrk/Worker.o Socket.o Dispatcher.o

Worker.o: wrk/Worker.c File/File.o wrk/WorkerCom.o

WorkerCom.o: wrk/WorkerCom.c wrk/WorkerCom.h Tabla/Tabla.o

WorkingTable.o: wrk/WorkingTable.c wrk/WorkingTable.h wrk/WorkingEntry.o

WorkingEntry.o: wrk/WorkingEntry.c wrk/WorkingEntry.h


clean: 
	rm *.o wrk/*.o File/*.o Tabla/*.o SList/*.o
	rm ./server
	rm ./wrk/worker
