-module(pera_processes).

-export([all/0]).
-export([to_json/1]).

%% @doc Return all processes running in the current node
-spec all() -> list(pid()).
all() ->
  erlang:processes().

to_json(Processes) ->
  jiffy:encode([list_to_binary(pid_to_list(Pid)) || Pid <- Processes]).
