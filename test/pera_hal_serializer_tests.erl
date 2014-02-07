-module(pera_hal_serializer_tests).
-include_lib("eunit/include/eunit.hrl").

empty_resource_to_json_test() ->
  JSON = pera_hal_serializer:to_json(pera_hal:resource([], [], [])),

  ?assert(JSON == <<"{\"_links\":{},\"_embedded\":{}}">>).

resource_with_properties_to_json_test() ->
  JSON = pera_hal_serializer:to_json(
    pera_hal:resource([], [], [{1, 2}])
  ),

  ?assert(JSON == <<"{\"_links\":{},\"_embedded\":{},\"1\":2}">>).

resource_with_links_to_json_test() ->
  JSON = pera_hal_serializer:to_json(
    pera_hal:resource(
      [pera_hal:link(<<"self">>, <<"/self">>, [])],
      [],
      []
    )
  ),

  ?assert(JSON == <<"{\"_links\":{\"self\":{\"href\":\"/self\"}},\"_embedded\":{}}">>).

resource_with_links_and_embedded_to_json_test() ->
  JSON = pera_hal_serializer:to_json(
    pera_hal:resource(
      [pera_hal:link(<<"self">>, <<"/self">>, [])],
      [{<<"thing">>, pera_hal:resource([pera_hal:link(<<"self">>, <<"/self/1">>, [])], [], [])}],
      []
    )
  ),


  ?assert(JSON == <<"{\"_links\":{\"self\":{\"href\":\"/self\"}},\"_embedded\":{\"thing\":{\"_links\":{\"self\":{\"href\":\"/self/1\"}},\"_embedded\":{}}}}">>).

resource_with_links_and_embeddeds_to_json_test() ->
  JSON = pera_hal_serializer:to_json(
    pera_hal:resource(
      [pera_hal:link(<<"self">>, <<"/self">>, [])],
      [
        {<<"things">>, [
            pera_hal:resource([pera_hal:link(<<"self">>, <<"/self/1">>, [])], [], []),
            pera_hal:resource([pera_hal:link(<<"self">>, <<"/self/2">>, [])], [], [])
          ]}
      ],
      []
    )
  ),

  ?assert(JSON == <<"{\"_links\":{\"self\":{\"href\":\"/self\"}},\"_embedded\":{\"things\":[{\"_links\":{\"self\":{\"href\":\"/self/1\"}},\"_embedded\":{}},{\"_links\":{\"self\":{\"href\":\"/self/2\"}},\"_embedded\":{}}]}}">>).
