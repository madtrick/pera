-module(pera_hal_link_data).
-include("pera.hrl").

-export([new/2]).
-export([get/2]).

%%========================================
%% API
%%========================================
-spec new(
  HREF    :: binary(),
  Options :: list(pera_hal_link_option())
  ) -> pera_hal_link().
new(HREF, Options) ->
  #pera_hal_link_data{
    href    = HREF,
    options = Options
  }.

-spec get(
  Key      :: atom(),
  LinkData :: pera_hal_link()
  ) ->
    {ok, any()} |
    undefined.
get(href, #pera_hal_link_data{ href = HREF }) -> ok(HREF);
get(templated, LinkData) -> get_optional(templated, LinkData);
get(type, LinkData) -> get_optional(type, LinkData);
get(deprecation, LinkData) -> get_optional(deprecation, LinkData);
get(name, LinkData) -> get_optional(name, LinkData);
get(profile, LinkData) -> get_optional(profile, LinkData);
get(title, LinkData) -> get_optional(title, LinkData);
get(hreflang, LinkData) -> get_optional(hreflang, LinkData);
get(_, _) -> undefined.

%%========================================
%% Internal
%%========================================
-spec ok(
  Value :: any()
  ) -> {ok, Value :: any()}.
ok(Value) -> {ok, Value}.

-spec get_optional(
  Key :: atom(),
  LinkData :: pera_hal_link()
) -> any().
get_optional(Key, LinkData) ->
  ok(proplists:get_value(Key, LinkData#pera_hal_link_data.options)).
