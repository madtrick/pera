-module(pera_hal_rel_data).
-include("pera.hrl").

-export([new/2]).
-export([get/2]).

-spec new(
  Name  :: atom(),
  Links :: pera_hal_link() | list(pera_hal_link())
  ) -> pera_hal_rel().
new(Name, Links) ->
  #pera_hal_rel_data{
    name  = Name,
    links = Links
  }.

-spec get(
  Key :: atom(),
  RelData  :: pera_hal_rel()
  ) ->
    {ok, any()} |
    undefined.
get(name, #pera_hal_rel_data{ name = Name }) -> ok(Name);
get(links, #pera_hal_rel_data{ links = Links }) -> ok(Links);
get(_, _) -> undefined.

%%========================================
%% Internal
%%========================================
-spec ok(
  Value :: any()
  ) -> {ok, Value :: any()}.
ok(Value) -> {ok, Value}.
