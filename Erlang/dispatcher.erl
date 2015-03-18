-module(dispatcher).
-export([listenTCP/1]).

-define(BUFF_SIZE, 1024).
-define(CS_PORT, 8000).
-define(TCP_OPTIONS, [binary, {reuseaddr, true}, {packet, 0}, {active, true}]).

listenTCP(ServerPID) -> case gen_tcp:listen(?CS_PORT , ?TCP_OPTIONS) of

                                {ok, LSock} -> listenTCP(ServerPID, LSock);

                                {error, Reason} -> io:format("Error al escuchar del socket: ~p~n", [Reason])
                        end.

listenTCP(Server, LSock) -> case gen_tcp:accept(LSock) of

                                        {ok, Socket} -> gen_tcp:controlling_process(Socket, Server),
                                                        Server ! {newClient, Socket},
                                                        listenTCP(Server, LSock);

                                        {error, Reason} -> io:format("Error al aceptar cliente desde el socket: ~p~n", [Reason]),
                                                           listenTCP(Server, LSock)
                            end.
