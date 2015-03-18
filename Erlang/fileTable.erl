-module(fileTable).
-compile(export_all).

-record(file, {name, data = ""}).

file_name(F)   -> F#file.name.
file_data(F)   -> F#file.data.
new_file(Name) -> #file{ name = Name }.

-record(tableEntry, {fd, name, worker, fdReal, id_owner, offset}).

tableEntry_fd(E)       -> E#tableEntry.fd.
tableEntry_name(E)     -> E#tableEntry.name.
tableEntry_id_owner(E) -> E#tableEntry.id_owner.
tableEntry_worker(E)   -> E#tableEntry.worker.
tableEntry_fdReal(E)   -> E#tableEntry.fdReal.
tableEntry_offset(E)   -> E#tableEntry.offset.
new_entry(Fd, Name, Worker, FdReal, ID, Offset) -> #tableEntry{ fd = Fd,
                                                                name = Name,
                                                                worker = Worker,
                                                                fdReal = FdReal,
                                                                id_owner = ID,
                                                                offset = Offset }.


the_file_is_on_the_table([], _)          -> false;
the_file_is_on_the_table([X | XS], Name) -> if 
                                                X#file.name == Name -> true;
                                                true                -> the_file_is_on_the_table(XS, Name)
                                            end.

select_file_from_table([], _)          -> null;
select_file_from_table([X | XS], Name) -> if 
                                                X#file.name == Name -> X;
                                                true                -> select_file_from_table(XS, Name)
                                          end.


select_entry_from_table_by_name([], _)            -> null;
select_entry_from_table_by_name([E | Rest], Name) -> if
                                                        E#tableEntry.name == Name -> E;
                                                        true                      -> select_entry_from_table_by_name(Rest, Name)
                                                     end.

select_entry_from_table_by_fd([], _)          -> null;
select_entry_from_table_by_fd([E | Rest], Fd) -> if
                                                        E#tableEntry.fd == Fd -> E;
                                                        true                  -> select_entry_from_table_by_fd(Rest, Fd)
                                                 end.


the_file_is_in_another_table([], _)            -> false;
the_file_is_in_another_table([P | Rest], Name) -> P ! {wrk_exs, Name, self()},
                                                  receive
                                                        {wrk_reply, "y"} -> true;
                                                        {wrk_reply, "n"} -> the_file_is_in_another_table(Rest, Name)
                                                  end.

update_offset([], _, _) -> [];
update_offset([E | Rest], Name, NewOffset) when E#tableEntry.name /= Name -> [E | update_offset(Rest, Name, NewOffset)];
update_offset([E | Rest], Name, NewOffset) when E#tableEntry.name == Name -> NewE = new_entry(tableEntry_fd(E), 
                                                                                               Name,
                                                                                               tableEntry_worker(E),
                                                                                               tableEntry_fdReal(E),
                                                                                               tableEntry_id_owner(E),
                                                                                               tableEntry_offset(E) + NewOffset),
                                                                             [NewE | Rest].

list_files([])         -> "";
list_files([F | Rest]) -> "\"" ++ file_name(F) ++ "\"" ++ " " ++ list_files(Rest).

list_extern_files([])         -> "";
list_extern_files([P | Rest]) -> P ! {wrk_lsd, self()},
                                 receive
                                        {wrk_reply, List} -> List ++ " " ++ list_extern_files(Rest)
                                 end.


delete_extern_file([], _)            -> e1;
delete_extern_file([P | Rest], Name) -> P ! {wrk_del, Name, self()},
                                        receive
                                                {wrk_reply, "ok"} -> ok;
                                                {wrk_reply, "e1"} -> delete_extern_file(Rest, Name);    %el archivo no existe en ese worker
                                                {wrk_reply, "e2"} -> e2                                 %no esta permitido
                                        end.

open_extern_file([], _, _)                   -> e1;
open_extern_file([P | Rest], Name, ClientID) -> P ! {wrk_opn, Name, ClientID, self()},
                                                receive
                                                          {wrk_reply, "ok", FD} -> {ok, FD, P};
                                                          {wrk_reply, "e1"}     -> open_extern_file(Rest, Name, ClientID);  %no existe el arch
                                                          {wrk_reply, "e2"}     -> e2                                       %no esta permitido
                                                end.

write_extern_file(Worker, Fd, Size, Data) -> Worker ! {wrk_wrt, Fd, Size, Data, self()},
                                             receive
                                                {wrk_reply, "ok"} -> ok
                                             end.

write_file([], _, _, _)                                           -> [];
write_file([F | Rest], Name, Data, Size) when F#file.name /= Name -> [F | write_file(Rest, Name, Data, Size)];
write_file([F | Rest], Name, Data, Size) when F#file.name == Name -> NewData = string:substr(Data, 1, Size),
                                                                     NewF = #file { name = Name, data = file_data(F) ++ NewData },
                                                                     [NewF | Rest].


read_extern_file(Worker, Fd, Size) -> Worker ! {wrk_rea, Fd, Size, self()},
                                      receive
                                            {wrk_reply, "ok", Result} -> Result
                                      end.

read_file([], _, _, _)                                             -> "";
read_file(_, _, Size, _)                  when Size < 0            -> "";
read_file([F | Rest], Name, Size, Offset) when F#file.name /= Name -> read_file(Rest, Name, Size, Offset);
read_file([F | _], Name, Size, Offset)    when F#file.name == Name -> if 
                                                                Size =< length(F#file.data) - Offset -> string:substr(file_data(F), Offset, Size);
                                                                true                                 -> string:substr(file_data(F), Offset)
                                                                       end.


close_file_by_ID([], _)          -> [];
close_file_by_ID([E | Rest], ID) -> case {tableEntry_id_owner(E), tableEntry_fdReal(E)} of
                                        {ID, -1} -> close_file_by_ID(Rest, ID);

                                        {ID, N}  -> tableEntry_worker(E) ! {wrk_clo, N, self()},
                                                    receive
                                                        {wrk_reply, "ok"} -> close_file_by_ID(Rest, ID)
                                                    end;
                                
                                        {_, _}   -> [E | close_file_by_ID(Rest, ID)]
                                    end.

close_file([], _)                                                                  -> [];
close_file([E | Rest], Fd) when E#tableEntry.fd == Fd, E#tableEntry.fdReal == -1 -> Rest;  % Es un archivo local
close_file([E | Rest], Fd) when E#tableEntry.fd == Fd, E#tableEntry.fdReal /= -1 -> tableEntry_worker(E) ! {wrk_clo, tableEntry_fdReal(E), self()},
                                                                                         receive
                                                                                                 {wrk_reply, "ok"} -> Rest
                                                                                         end;
close_file([E | Rest], Fd)                                                         -> [E | close_file(Rest, Fd)].
