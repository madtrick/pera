-module(pera_hal_resource_data_tests).
-include_lib("eunit/include/eunit.hrl").

hal_resource_only_links_test() ->
  Links = [pera_hal:link("self", "/resource", [])],
  Resource = pera_hal_resource_data:new(Links, [], []),

  ?assert(pera_hal_resource_data:get(links, Resource) == {ok, Links}),
  ?assert(pera_hal_resource_data:get(embedded, Resource) == {ok, []}),
  ?assert(pera_hal_resource_data:get(properties, Resource) == {ok, []}).

hal_resource_embedded_test() ->
  Links = [pera_hal:link("self", "/resource", [])],
  Embedded = pera_hal_resource_data:new([], [], []),
  Resource = pera_hal_resource_data:new(Links, Embedded, []),

  ?assert(pera_hal_resource_data:get(links, Resource) == {ok, Links}),
  ?assert(pera_hal_resource_data:get(embedded, Resource) == {ok, Embedded}),
  ?assert(pera_hal_resource_data:get(properties, Resource) == {ok, []}).

hal_resource_properties_test() ->
  Properties = [{"property", value}],
  Resource = pera_hal_resource_data:new([], [], Properties),

  ?assert(pera_hal_resource_data:get(links, Resource) == {ok, []}),
  ?assert(pera_hal_resource_data:get(embedded, Resource) == {ok, []}),
  ?assert(pera_hal_resource_data:get(properties, Resource) == {ok, Properties}).


