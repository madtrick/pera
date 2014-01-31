-module(pera_processes).
-include("pera.hrl").

-export([all/0]).
-export([all_registered/0]).

%% @doc Return all processes running in the current node
-spec all() -> #pera_response{}.
all() ->
  pera_response:new(erlang:processes(), pera_processes_serializer).

%% @doc Return all the registerd processes in the current node
-spec all_registered() -> #pera_response{}.
all_registered() ->
  pera_response:new(erlang:registered(), pera_processes_serializer).
