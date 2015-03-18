-module(server).
-export([server_init/0, server_sync/0, socket_process/3, handle_client/2]).

-define(N_WORKERS, 5).
-define(WELCOME, " ---------------------------------------------------\n| MISAD: [MI] [S]istema de [A]rchivos [D]istribuido |\n ---------------------------------------------------\nBienvenido..\n").

% ADICIONAL: 
% Re-inicio del grupo de workers si se cae alguno. Aquı los pedidos que fueron realizados se perderan y el FS se reinicializara vacıo.
%

% Devuelvo la lista con los pids de los workers
load_workers(0, _) -> [];
load_workers(N, Data) -> [spawn(worker, worker_init, [Data]) | load_workers(N-1, Data)].


send_PIDs(List) -> send_PIDs(List, List).

send_PIDs(_, []) -> ok;
send_PIDs(List, [P | T]) -> NewList = lists:filter(fun(E) -> E /= P end, List),
                            P ! {workers, NewList, self()},
                            send_PIDs(List, T).


confirm_workers(WList)    -> confirm_workers(WList, ?N_WORKERS).
confirm_workers(_, 0)     -> ok;
confirm_workers(WList, N) -> receive
                                        workerOK    -> confirm_workers(WList, N-1);
                                        workerERROR -> error
                             end.


exit_workers([])         -> ok;
exit_workers([P | Rest]) -> exit(P, kill),
                            exit_workers(Rest).

monitor_workers([])         -> [];
monitor_workers([P | Rest]) -> [monitor(process, P) | monitor_workers(Rest)].

demonitor_workers([])         -> ok;
demonitor_workers([R | Rest]) -> demonitor(R),
                                 demonitor_workers(Rest).

server_sync() -> receive
                         {lock, Pid} -> Pid ! locked,
                                        receive
                                               unlock -> Pid ! unlocked
                                        end,
                                        server_sync()
                 end.


server_init() -> ListenPid = spawn(dispatcher, listenTCP, [self()]),
                 SyncPid = spawn(server, server_sync, []),
                 WorkerList = load_workers(?N_WORKERS, SyncPid),
                 send_PIDs(WorkerList),
                 case confirm_workers(WorkerList) of
                        ok    -> io:format("Workers cargados...~n", []),
                                 MonRefs = monitor_workers(WorkerList),
                                 server(WorkerList, 1, MonRefs, ListenPid, SyncPid);

                        error -> io:format("Error cargando workers...~n", []),
                                 exit(ListenPid, kill),
                                 exit(SyncPid, kill),
                                 exit_workers(WorkerList),
                                 server_init()                                  % termino los procesos iniciados y reintento iniciar el servidor
                 end.


server(WorkerList, ID_ref, MonRefs, ListenPid, SyncPid) -> receive
                               {newClient, Socket}               -> N = random:uniform(?N_WORKERS),
                                                                    P = spawn(?MODULE, handle_client, [lists:nth(N, WorkerList), ID_ref]),
                                                                    gen_tcp:controlling_process(Socket, P),
                                                                    server(WorkerList, ID_ref+1, MonRefs, ListenPid, SyncPid);

                               {'DOWN', _, process, Pid, Reason} -> io:format("Worker caido: ~p~nCargando nuevos workers...~n",[Reason]),
                                                                    demonitor_workers(MonRefs),
                                                                    exit_workers(lists:filter(fun(E) -> E /= Pid end, WorkerList)),
                                                                    exit(ListenPid, kill),
                                                                    exit(SyncPid, kill),
                                                                    server_init()
                                                            end.


handle_client(Worker, ID) -> receive
 
                                       {tcp, ClientSocket, <<"CON",R/binary>>} -> if 
                                                                                (R /= <<"\r\n">>) and 
                                                                                (R /= <<"\r">>) and
                                                                                (R /= <<"\n">>) -> io:format("Error de conexion de nuevo cliente~n", []);

                                                                                true            -> io:format("Nuevo cliente conectado~n", []),
                                                                                                   socket_process(ClientSocket, Worker, ID)
                                                                                  end;

                                       {tcp, _, _}                             -> io:format("Error de conexion de nuevo cliente~n", []);

                                       {tcp_closed, _}                         -> io:format("Conexion con cliente cerrada~n", []);

                                       {tcp_error, _, Reason}                  -> io:format("Error de recepcion: ~p~n", [Reason])

                             after 10000 -> io:format("Conexion con cliente cerrada por tiempo excedido~n", []),
                                            exit(normal)

                             end.

socket_process(Socket, Worker, ID) -> Worker ! {newClient, Socket, ID, self()},
                                      receive
                                                 {ok, Pid} -> gen_tcp:send(Socket, ?WELCOME),
                                                              socket_process_loop(Pid)

                                      after 10000 -> gen_tcp:send(Socket, "Worker caido, intente reconectarse\n"),
                                                     gen_tcp:close(Socket)
                                      end.

socket_process_loop(Worker) -> receive
                                        {tcp, ClientSocket, <<"BYE",_/binary>>} -> gen_tcp:close(ClientSocket),
                                                                                   Worker ! <<"BYE\n">>;

                                        {tcp, ClientSocket, Bin}                -> Worker ! Bin,
                                                                                   receive
                                                                                        {reply, Result} -> gen_tcp:send(ClientSocket, Result)
                                                                                   end,
                                                                                   socket_process_loop(Worker);

                                        {tcp_closed, _}              -> Worker ! <<"BYE\n">>,
                                                                        io:format("Conexion con cliente cerrada~n", []);

                                        {tcp_error, _, Reason}       -> Worker ! <<"BYE\n">>,
                                                                        io:format("Error de recepcion: ~p~n", [Reason])
                               end.

