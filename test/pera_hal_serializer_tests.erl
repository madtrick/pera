-module(pera_hal_serializer_tests).
-include_lib("eunit/include/eunit.hrl").
-include("pera_hal_macros.hrl").

empty_resource_to_json_test() ->
  JSON = pera_hal_serializer:to_json(?HAL_RESOURCE([], [], [])),

  ?assert(JSON == <<"{\"_links\":{},\"_embedded\":{}}">>).

resource_with_properties_to_json_test() ->
  JSON = pera_hal_serializer:to_json(
    ?HAL_RESOURCE([], [], [?HAL_PROPERTY_OBJECT([{1, 2}])])
  ),

  ?assert(JSON == <<"{\"_links\":{},\"_embedded\":{},\"1\":2}">>).

resource_with_nested_properties_to_json_test() ->
  JSON = pera_hal_serializer:to_json(
    ?HAL_RESOURCE(
      [],
      [],
      [
        ?HAL_PROPERTY_OBJECT([
            {1, ?HAL_PROPERTY_OBJECT([
                  {2, 3}
                ])
            }
          ])
      ]
    )
  ),

  ?assert(JSON == <<"{\"_links\":{},\"_embedded\":{},\"1\":{\"2\":3}}">>).

resource_with_list_in_property_to_json_test() ->
  JSON = pera_hal_serializer:to_json(
    ?HAL_RESOURCE(
      [],
      [],
      [?HAL_PROPERTY_OBJECT([ {1, [2, 3, 4]} ])]
    )
  ),

  ?assert(JSON == <<"{\"_links\":{},\"_embedded\":{},\"1\":[2,3,4]}">>).

resource_with_nested_collection_of_objects_to_json_test() ->
  JSON = pera_hal_serializer:to_json(
    ?HAL_RESOURCE(
      [],
      [],
      [
        ?HAL_PROPERTY_OBJECT([ {1, [
                ?HAL_PROPERTY_OBJECT([{2, 3}]),
                ?HAL_PROPERTY_OBJECT([{4, 5}])
              ]}
          ])
      ]
    )
  ),

  ?assert(JSON == <<"{\"_links\":{},\"_embedded\":{},\"1\":[{\"2\":3},{\"4\":5}]}">>).

resource_with_links_to_json_test() ->
  JSON = pera_hal_serializer:to_json(
    ?HAL_RESOURCE(
      [?HAL_LINK(self, <<"/self">>, [])],
      [],
      []
    )
  ),

  ?assert(JSON == <<"{\"_links\":{\"self\":{\"href\":\"/self\"}},\"_embedded\":{}}">>).

resource_with_links_and_embedded_to_json_test() ->
  JSON = pera_hal_serializer:to_json(
    ?HAL_RESOURCE(
      [?HAL_LINK(self, <<"/self">>, [])],
      [?HAL_EMBEDDED(<<"thing">>, ?HAL_RESOURCE([?HAL_LINK(self, <<"/self/1">>, [])], [], []))],
      []
    )
  ),


  ?assert(JSON == <<"{\"_links\":{\"self\":{\"href\":\"/self\"}},\"_embedded\":{\"thing\":{\"_links\":{\"self\":{\"href\":\"/self/1\"}},\"_embedded\":{}}}}">>).

resource_with_links_and_embeddeds_to_json_test() ->
  JSON = pera_hal_serializer:to_json(
    ?HAL_RESOURCE(
      [?HAL_LINK(self, <<"/self">>, [])],
      [?HAL_EMBEDDED(<<"things">>,
          [
            ?HAL_RESOURCE([?HAL_LINK(self, <<"/self/1">>, [])], [], []),
            ?HAL_RESOURCE([?HAL_LINK(self, <<"/self/2">>, [])], [], [])
          ]
        )
      ],
      []
    )
  ),

  ?assert(JSON == <<"{\"_links\":{\"self\":{\"href\":\"/self\"}},\"_embedded\":{\"things\":[{\"_links\":{\"self\":{\"href\":\"/self/1\"}},\"_embedded\":{}},{\"_links\":{\"self\":{\"href\":\"/self/2\"}},\"_embedded\":{}}]}}">>).
