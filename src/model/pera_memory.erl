-module(pera_memory).

-export([find/1]).

-spec find(
  TypeList :: list(atom())
  ) -> list({atom(), pos_integer()}).
find(_) ->
  erlang:memory().
