-module(worker).
-export([worker_init/1, worker_handle_client/5]).

-define(EPERM, 1).
-define(ENOENT, 2).
-define(ENOOPN, 3).
-define(EAGAIN, 11).
-define(EEXIST, 17).
-define(USAGE, "Informacion de los comandos:\n-> LSD\n\tlistado de archivos existentes.\n-> CRE file\n\tcrea el archivo \"file\" en el sistema.\n-> DEL file\n\telimina el archivo \"file\" del sistema.\n-> OPN file\n\tabre el archivo \"file\" para escritura y lectura.\n-> CLO FD n\n\tcierra el archivo con descriptor \"n\".\n-> REA FD n SIZE m\n\tlee \"m\" bytes del archivo con descriptor \"n\".\n-> WRT FD n SIZE m s\n\tescribe \"m\" bytes de \"s\" en el archivo con descriptor \"n\".\n").

worker_init(Server) -> receive
                            {workers, [], From}      -> From ! workerERROR;

                            {workers, PIDList, From} -> From ! workerOK,
                                                        worker(Server, PIDList, [], [])							
                       end.

worker(Server, PIDList, LocalFT, LocalWT) -> 
        receive
                {newClient, Socket, ID, FromPid} -> P = spawn(worker, worker_handle_client, [Socket, ID, self(), FromPid, Server]),
                                                    FromPid ! {ok, P},
                                                    worker(Server, PIDList, LocalFT, LocalWT);

		{lsd, Pid} -> Localfiles = fileTable:list_files(LocalFT),
                              Externfiles = fileTable:list_extern_files(PIDList),
                              Pid ! {ok, string:strip(Localfiles, right) ++ Externfiles ++ "\n"},
                              worker(Server, PIDList, LocalFT, LocalWT);

                {del, Name, Pid} -> case fileTable:select_file_from_table(LocalFT, Name) of
                                            null -> case fileTable:delete_extern_file(PIDList, Name) of
                                                           ok -> Pid ! {ok, "\n"};
                                                           e1 -> Pid ! {error, ?ENOENT};
                                                           e2 -> Pid ! {error, ?EPERM}
                                                    end,
                                                    worker(Server, PIDList, LocalFT, LocalWT);
                                
                                            F    -> case fileTable:select_entry_from_table_by_name(LocalWT, Name) of
                                                           null -> Pid ! {ok, "\n"},
                                                                   worker(Server, PIDList, lists:delete(F, LocalFT), LocalWT);

                                                           _    -> Pid ! {error, ?EPERM},
                                                                   worker(Server, PIDList, LocalFT, LocalWT)
                                                    end
                                    end;

                {cre, Name, Pid} -> server_lock(Server),
				    File = fileTable:select_file_from_table(LocalFT, Name),
                                    Exist = fileTable:the_file_is_in_another_table(PIDList, Name),
                                    if
                                            File /= null ; Exist  -> Pid ! {error, ?EEXIST},
								     server_unlock(Server),
                                                                     worker(Server, PIDList, LocalFT, LocalWT);

                                            true                  -> Newfile = fileTable:new_file(Name),
                                                                     Pid ! {ok, "\n"},
								     server_unlock(Server),
                                                                     worker(Server, PIDList, [Newfile | LocalFT], LocalWT)
                                    end;
        
                {opn, Name, ID, Pid} -> case fileTable:select_file_from_table(LocalFT, Name) of
                                              null -> case fileTable:open_extern_file(PIDList, Name, ID) of
                                                            {ok, FD, Worker} -> NewEntry = fileTable:new_entry(length(LocalWT), Name, Worker, FD, ID, 1),
                                                                                Pid ! {ok, integer_to_list(fileTable:tableEntry_fd(NewEntry)) ++ "\n"},
                                                                                worker(Server, PIDList, LocalFT, [NewEntry | LocalWT]);
                                                            e1               -> Pid ! {error, ?ENOENT},
                                                                                worker(Server, PIDList, LocalFT, LocalWT);
                                                            e2               -> Pid ! {error, ?EPERM},
                                                                                worker(Server, PIDList, LocalFT, LocalWT)
                                                      end;

                                              _    -> case fileTable:select_entry_from_table_by_name(LocalWT, Name) of
                                                            null -> NewEntry = fileTable:new_entry(length(LocalWT), Name, self(), -1, ID, 1),
                                                                    Pid ! {ok, integer_to_list(fileTable:tableEntry_fd(NewEntry)) ++ "\n"},
                                                                    worker(Server, PIDList, LocalFT, [NewEntry | LocalWT]);

                                                            E    -> case fileTable:tableEntry_id_owner(E) of

                                                                        ID -> Pid ! {ok, integer_to_list(fileTable:tableEntry_fd(E)) ++ "\n"},
                                                                              worker(Server, PIDList, LocalFT, LocalWT);

                                                                        _  -> Pid ! {error, ?EPERM},
                                                                              worker(Server, PIDList, LocalFT, LocalWT)
                                                                    end
                                                       end
                                              end;

                {wrt, Fd, Size, Data, ID, Pid} -> case fileTable:select_entry_from_table_by_fd(LocalWT, Fd) of
                                                                null -> Pid ! {error, ?ENOOPN},
                                                                        worker(Server, PIDList, LocalFT, LocalWT);
  
                                                                E    -> case {fileTable:tableEntry_id_owner(E), fileTable:tableEntry_fdReal(E)}  of
                                                
                                                                          {ID, -1} -> NewLocalFT = fileTable:write_file(LocalFT, 
                                                                                                                        fileTable:tableEntry_name(E),
                                                                                                                        Data, 
                                                                                                                        Size),
                                                                                      Pid ! {ok, "\n"},
                                                                                      worker(Server, PIDList, NewLocalFT, LocalWT);
              
                                                                          {ID, _}  -> fileTable:write_extern_file(fileTable:tableEntry_worker(E),
                                                                                                             fileTable:tableEntry_fdReal(E), Size, Data),
                                                                                      Pid ! {ok, "\n"},
                                                                                      worker(Server, PIDList, LocalFT, LocalWT);
                                                                                      
                                                                          {_, _}   -> Pid ! {error, ?EPERM},
                                                                                      worker(Server, PIDList, LocalFT, LocalWT)
                                                                        end
                                                  end;

                {rea, Fd, Size, ID, Pid} -> case fileTable:select_entry_from_table_by_fd(LocalWT, Fd) of
                                                       null -> Pid ! {error, ?ENOOPN},
                                                               worker(Server, PIDList, LocalFT, LocalWT);
  
                                                       E    -> case {fileTable:tableEntry_id_owner(E), fileTable:tableEntry_fdReal(E)} of
                                                                {ID, -1} -> Result = fileTable:read_file(LocalFT,
                                                                                                         fileTable:tableEntry_name(E), 
                                                                                                         Size,
                                                                                                         fileTable:tableEntry_offset(E)),
                                                                            Pid ! {ok, Result ++ "\n"},
                                                                            NewLocalWT = fileTable:update_offset(LocalWT,
                                                                                                                 fileTable:tableEntry_name(E), 
                                                                                                                 length(Result)),
                                                                            worker(Server, PIDList, LocalFT, NewLocalWT);
              
                                                                {ID, _}  -> Result = fileTable:read_extern_file(fileTable:tableEntry_worker(E), 
                                                                                                                fileTable:tableEntry_fdReal(E),
                                                                                                                Size),
                                                                            Pid ! {ok, Result ++ "\n"},
                                                                            NewLocalWT = fileTable:update_offset(LocalWT,
                                                                                                                 fileTable:tableEntry_name(E), 
                                                                                                                 length(Result)),
                                                                            worker(Server, PIDList, LocalFT, NewLocalWT);

                                                                {_, _}   -> Pid ! {error, ?EPERM},
                                                                            worker(Server, PIDList, LocalFT, LocalWT)
                                                               end
                                            end;

                {clo, Fd, ID, Pid} -> case fileTable:select_entry_from_table_by_fd(LocalWT, Fd) of
                                        null -> Pid ! {error, ?ENOOPN},
                                                worker(Server, PIDList, LocalFT, LocalWT);

                                        E    -> case fileTable:tableEntry_id_owner(E) of
                                                        ID -> NewLocalWT = fileTable:close_file(LocalWT, fileTable:tableEntry_fd(E)),
                                                              Pid ! {ok, "\n"},
                                                              worker(Server, PIDList, LocalFT, NewLocalWT);

                                                        _  -> Pid ! {error, ?EPERM},
                                                              worker(Server, PIDList, LocalFT, LocalWT)
                                                end
                                      end;

                {bye, ID, _} -> worker(Server, PIDList, LocalFT, fileTable:close_file_by_ID(LocalWT, ID));

                {wrk_lsd, Pid} -> Result = fileTable:list_files(LocalFT),
                                  Pid ! {wrk_reply, Result},
                                  worker(Server, PIDList, LocalFT, LocalWT);

                {wrk_del, Name, Pid} -> case fileTable:select_file_from_table(LocalFT, Name) of
                                                null -> Pid ! {wrk_reply, "e1"},
                                                        worker(Server, PIDList, LocalFT, LocalWT);
                                                F    -> case fileTable:select_entry_from_table_by_name(LocalWT, Name) of
                                                                null -> Pid ! {wrk_reply, "ok"},
                                                                        worker(Server, PIDList, lists:delete(F, LocalFT), LocalWT);
                                                                _    -> Pid ! {wrk_reply, "e2"},
                                                                        worker(Server, PIDList, LocalFT, LocalWT)
                                                        end
                                        end;
                {wrk_opn, Name, ID, Pid} -> case fileTable:select_file_from_table(LocalFT, Name) of
                                                null -> Pid ! {wrk_reply, "e1"},
                                                        worker(Server, PIDList, LocalFT, LocalWT);
                                                _    -> case fileTable:select_entry_from_table_by_name(LocalWT, Name) of
                                                                null -> NewEntry = fileTable:new_entry(length(LocalWT), Name, Pid, -1, ID, 1),
                                                                        Pid ! {wrk_reply, "ok", fileTable:tableEntry_fd(NewEntry)},
                                                                        worker(Server, PIDList, LocalFT, [NewEntry | LocalWT]);
                                                                
                                                                E    -> case fileTable:tableEntry_id_owner(E) of
                                                                                ID -> Pid ! {wrk_reply, "ok", fileTable:tableEntry_fd(E)},
                                                                                      worker(Server, PIDList, LocalFT, LocalWT);

                                                                                _  -> Pid ! {wrk_reply, "e2"},
                                                                                      worker(Server, PIDList, LocalFT, LocalWT)
                                                                        end
                                                        end
                                            end;                                                               

                {wrk_exs, Name, Pid} -> case fileTable:select_file_from_table(LocalFT, Name) of
                                                null -> Pid ! {wrk_reply, "n"};
                                                _    -> Pid ! {wrk_reply, "y"}
                                        end,
                                        worker(Server, PIDList, LocalFT, LocalWT);

                {wrk_wrt, Fd, Size, Data, Pid} -> Entry = fileTable:select_entry_from_table_by_fd(LocalWT, Fd),
                                                  NewLocalFT = fileTable:write_file(LocalFT, fileTable:tableEntry_name(Entry), Data, Size),
                                                  Pid ! {wrk_reply, "ok"},
                                                  worker(Server, PIDList, NewLocalFT, LocalWT);

                {wrk_rea, Fd, Size, Pid} -> Entry = fileTable:select_entry_from_table_by_fd(LocalWT, Fd),
                                            Result=fileTable:read_file(LocalFT,fileTable:tableEntry_name(Entry),Size,fileTable:tableEntry_offset(Entry)),
                                            NewLocalWT = fileTable:update_offset(LocalWT, fileTable:tableEntry_name(Entry), length(Result)),
                                            Pid ! {wrk_reply, "ok", Result},
                                            worker(Server, PIDList, LocalFT, NewLocalWT);

                {wrk_clo, Fd, Pid} -> NewLocalWT = fileTable:close_file(LocalWT, Fd),
                                      Pid ! {wrk_reply, "ok"},
                                      worker(Server, PIDList, LocalFT, NewLocalWT)
        end,
        worker(Server, PIDList, LocalFT, LocalWT).


worker_handle_client(Socket, ClientID, Worker, SocketProc, Server) -> 
        receive
                   <<"LSD\r\n">>             -> Worker ! {lsd, self()};

                   <<"DEL ",Name/binary>>    -> Worker ! {del, string:strip((binary_to_list(Name) -- "\n") -- "\r", both), self()};

                   <<"CRE ",Name/binary>>    -> if
                                        (Name == <<"\r\n">>) or 
                                        (Name == <<"\n">>) or
                                        (Name == <<"\r">>)     -> SocketProc ! {reply, "ERROR 0 comando incorrecto, ingrese HLP para mas información\n"},
                                                                  worker_handle_client(Socket, ClientID, Worker, SocketProc, Server);

                                        true                   -> Worker ! {cre, string:strip((binary_to_list(Name) -- "\n") -- "\r", both), self()}
                                                end;

                   <<"OPN ",Name/binary>>    -> Worker ! {opn, string:strip((binary_to_list(Name) -- "\n") -- "\r", both), ClientID, self()};

                   <<"WRT FD ",Args/binary>> -> case string:tokens((binary_to_list(Args) -- "\n") -- "\r", " ") of
                                                      [Fd, "SIZE", N | Data] -> Worker ! {wrt, list_to_integer(Fd), list_to_integer(N), string:join(Data, " "), ClientID, self()};

                                                      _ -> SocketProc ! {reply, "ERROR 0 comando incorrecto, ingrese HLP para mas información\n"},
                                                           worker_handle_client(Socket, ClientID, Worker, SocketProc, Server)
                                                end;

                   <<"REA FD ",Args/binary>> -> case string:tokens((binary_to_list(Args) -- "\n") -- "\r", " ") of
                                                      [Fd, "SIZE", N] -> Worker ! {rea, list_to_integer(Fd), list_to_integer(N), ClientID, self()};

                                                      _ -> SocketProc ! {reply, "ERROR 0 comando incorrecto, ingrese HLP para mas información\n"},
                                                           worker_handle_client(Socket, ClientID, Worker, SocketProc, Server)
                                                end;

                   <<"CLO FD ", Fd/binary>>  -> Worker ! {clo, list_to_integer((binary_to_list(Fd) -- "\n") -- "\r"), ClientID, self()};
               
                   <<"BYE",_/binary>>        -> Worker ! {bye, ClientID, self()},
                                                exit(normal);

                   <<"HLP",_/binary>>        -> SocketProc ! {reply, ?USAGE},
                                                worker_handle_client(Socket, ClientID, Worker, SocketProc, Server);

                   _                         -> SocketProc ! {reply, "ERROR 0 comando incorrecto, ingrese HLP para mas información\n"},
                                                worker_handle_client(Socket, ClientID, Worker, SocketProc, Server)
        end,
        receive
                {ok, Reply}   -> Result = "OK " ++ string:strip(Reply, left);
                {error, Desc} -> Result = error_desc(Desc)
        end,
        SocketProc ! {reply, Result},
        worker_handle_client(Socket, ClientID, Worker, SocketProc, Server).


server_lock(Server) -> Server ! {lock, self()},
                       receive
                              locked -> ok
                       end.

server_unlock(Server) -> Server ! unlock,
                         receive
                                unlocked -> ok
                         end.


error_desc(N) -> case N of
                        ?EPERM  -> "ERROR 1 Operacion no permitida\n";
                        ?ENOENT -> "ERROR 2 No existe el archivo\n";
                        ?ENOOPN -> "ERROR 3 El archivo no esta abierto\n";
                        ?EEXIST -> "ERROR 17 El archivo ya existe\n";
                        ?EAGAIN -> "ERROR 11 Fallo la operacion, intente nuevamente\n"
                 end.
