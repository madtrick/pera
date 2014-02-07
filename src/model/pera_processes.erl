-module(pera_processes).
-include("pera.hrl").

-export([all/0]).
-export([all_registered/0]).

%% @doc Return all processes running in the current node
-spec all() -> list(pid()).
all() ->
  erlang:processes().

%% @doc Return all the registerd processes in the current node
-spec all_registered() -> list(atom()).
all_registered() ->
  erlang:registered().
