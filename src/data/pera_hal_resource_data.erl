-module(pera_hal_resource_data).
-include("pera.hrl").

-export([new/3]).
-export([get/2]).

%%========================================
%% API
%%========================================
-spec new(
  Rels            :: list(pera_hal_rel()),
  Embedded        :: pera_hal_embedded() | list(pera_hal_embedded()),
  PropertyObjects :: list(pera_hal_resource_property_object())
  ) -> pera_hal_resource().
new(Rels, Embedded, PropertiesObject) ->
  #pera_hal_resource_data{
    rels       = Rels,
    embedded   = Embedded,
    properties = PropertiesObject
  }.

-spec get(
  Key          :: atom(),
  ResourceData :: pera_hal_resource()
  ) ->
  {ok,
    list(pera_hal_rel()) |
    pera_hal_embedded() |
    list(pera_hal_embedded()) |
    list(pera_hal_resource_property_object())
  } |
  undefined.
get(rels, #pera_hal_resource_data{ rels = Value }) -> {ok, Value};
get(embedded, #pera_hal_resource_data{ embedded = Value }) -> {ok, Value};
get(properties, #pera_hal_resource_data{ properties = Value }) -> {ok, Value};
get(_, _) -> undefined.
