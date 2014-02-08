-module(pera_json).

%% @hidden
%% @doc Module to abstract the JSON generation

-export([object/1]).
-export([array/1]).
-export([to_json/1]).

%%========================================
%% Types
%%========================================
-type json_key()      :: atom() | binary() | integer() | string().
-type json_value()    :: atom() | binary() | boolean() | integer() | list() | json_object() | json_array().
-type json_property() :: {json_key(), json_value()}.
-type json_object()   :: {object, json_impl_object()}.
-type json_array()    :: {array, json_impl_array()}.

-export_type([json_key/0]).
-export_type([json_value/0]).
-export_type([json_property/0]).
-export_type([json_object/0]).
-export_type([json_array/0]).

%% This types are implementation dependent (Jiffy currently)
-type json_impl_key()    :: binary() | atom().
-type json_impl_value()  :: binary() | atom() | boolean() | integer() | list() | json_impl_object() | json_impl_array().
-type json_impl_object() :: {[]} | {list({json_impl_key(), json_impl_value()})}.
-type json_impl_array()  :: list(json_impl_value()).

%%========================================
%% API
%%========================================

%% @doc Returns a intermediate representation of a JSON object
%%
%% This intermediate representation has to be passed to {@link to_json/1}
%% to generate a JSON string
-spec object(
  Properties :: list(json_property())
  ) -> json_object().
object(Properties) ->
  {object, {[build_property(Property) || Property <- Properties]}}.

%% @doc Returns a intermediate representation of a JSON array
%%
%% This intermediate representation has to be passed to {@link to_json/1}
%% to generate a JSON string
-spec array(
  Values :: list(json_value())
  ) -> json_array().
array(Values) ->
  {array, [build_value(Value) || Value <- Values]}.

%% @doc Generates a string with the JSON representation
-spec to_json(
  Object :: json_object()
  ) -> binary()
  ;
  (
  Array :: json_array()
  ) -> binary().
to_json({object, Object}) ->
  jiffy:encode(Object);
to_json({array, Array}) ->
  jiffy:encode(Array).


%%========================================
%% Internal
%%========================================
-spec build_property(
  Property :: json_property()
  ) -> {json_impl_key(), json_impl_value()}.
build_property({Key, Value})->
  {build_key(Key), build_value(Value)}.

-spec build_key(
  Key :: json_key()
) ->
  atom() |
  binary().
build_key(Key) when is_binary(Key) ->
  Key;
build_key(Key) when is_list(Key) ->
  erlang:list_to_binary(Key);
build_key(Key) when is_atom(Key) ->
  Key;
build_key(Key) when is_integer(Key) ->
  erlang:integer_to_binary(Key).

-spec build_value(
  Value :: json_value()
  ) -> json_impl_value().
build_value({object, Object}) ->
  Object;
build_value({array, Array}) ->
  Array;
build_value(Value) ->
  Value.
