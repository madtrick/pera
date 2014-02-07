-module(pera_json_tests).
-include_lib("eunit/include/eunit.hrl").

create_empty_object_test() ->
  Object = pera_json:object([]),

  ?assert(Object == {object, {[]}}).

create_object_test() ->
  Object = pera_json:object([{p1, v1}, {<<"p2">>, v2}, {<<"p3">>, "v3"} ]),

  ?assert(Object == {object, {[{p1, v1}, {<<"p2">>, v2}, {<<"p3">>, "v3"}]}}).

object_to_json_test() ->
  JSON = pera_json:to_json(pera_json:object([{p1, v1}])),

  ?assert(JSON == <<"{\"p1\":\"v1\"}">>).

create_array_test() ->
  Array = pera_json:array([1, 2]),

  ?assert(Array == {array, [1, 2]}).

array_to_json_test() ->
  JSON = pera_json:to_json(pera_json:array([1, 2])),

  ?assert(JSON == <<"[1,2]">>).

complex_object_to_json_test() ->
  JSON = pera_json:to_json(
    pera_json:object([{1, 2}, {a, pera_json:object([{3, 4}])}])
  ),

  ?assert(JSON == <<"{\"1\":2,\"a\":{\"3\":4}}">>).

