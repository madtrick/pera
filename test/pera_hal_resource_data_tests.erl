-module(pera_hal_resource_data_tests).
-include_lib("eunit/include/eunit.hrl").

hal_resource_only_links_test() ->
  Rels = [pera_hal:rel(self, pera_hal:link("/resource", []))],
  Resource = pera_hal_resource_data:new(Rels, [], []),

  ?assert(pera_hal_resource_data:get(rels, Resource) == {ok, Rels}),
  ?assert(pera_hal_resource_data:get(embedded, Resource) == {ok, []}),
  ?assert(pera_hal_resource_data:get(properties, Resource) == {ok, []}).

hal_resource_embedded_test() ->
  Rels = [pera_hal:rel(self, pera_hal:link("/resource", []))],
  Embedded = pera_hal_resource_data:new([], [], []),
  Resource = pera_hal_resource_data:new(Rels, Embedded, []),

  ?assert(pera_hal_resource_data:get(rels, Resource) == {ok, Rels}),
  ?assert(pera_hal_resource_data:get(embedded, Resource) == {ok, Embedded}),
  ?assert(pera_hal_resource_data:get(properties, Resource) == {ok, []}).

hal_resource_properties_test() ->
  Properties = [{"property", value}],
  Resource = pera_hal_resource_data:new([], [], Properties),

  ?assert(pera_hal_resource_data:get(rels, Resource) == {ok, []}),
  ?assert(pera_hal_resource_data:get(embedded, Resource) == {ok, []}),
  ?assert(pera_hal_resource_data:get(properties, Resource) == {ok, Properties}).


