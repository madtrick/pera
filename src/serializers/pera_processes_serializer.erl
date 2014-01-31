-module(pera_processes_serializer).

-export([to_json/1]).

-spec to_json(
  Data :: list(pid())
  ) -> binary()
  ;
  (Data :: list(atom())
  ) -> binary().
to_json(Data = [H | _]) when is_pid(H) ->
  jiffy:encode([list_to_binary(pid_to_list(Pid)) || Pid <- Data]);
to_json(Data) ->
  jiffy:encode([atom_to_binary(Name, utf8) || Name <- Data]).
