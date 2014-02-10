-module(pera_rep_modules).
-include("pera_hal_macros.hrl").

%% @hidden
%% @doc Module that returns a representation
%% for the modules loaded in the system
%%
%% Currenty only the HAL format is supported.

-export([to_hal/2]).

%%========================================
%% API
%%========================================

%% @doc Returns a representation for the modules loaded
-spec to_hal(
  Modules :: list({module(), atom() | string()}),
  Options :: list({atom(), any()})
  ) -> binary().
to_hal(Modules, _) ->
  Resource = ?HAL_RESOURCE(
    [
      ?HAL_REL(self, ?HAL_LINK(<<"/modules">>, [])),
      ?HAL_REL(curies, ?HAL_LINK(<<"/relations/{rel}">>,[{templated, true}, {name, pera}])),
      ?HAL_REL('pera:module', ?HAL_LINK(<<"/modules/{module}">>, [{templated, true}]))
    ],
    [],
    [?HAL_PROPERTY_OBJECT([?HAL_PROPERTY(Key, ensure_atom_or_binary_value(Value)) || {Key, Value} <- Modules])]
  ),

  pera_hal_serializer:to_json(Resource).

%%========================================
%% Internal
%%========================================
-spec ensure_atom_or_binary_value(
  Value :: string() | atom()
  ) -> binary() | atom().
ensure_atom_or_binary_value(Value) when is_atom(Value) ->
  Value;
ensure_atom_or_binary_value(Value) ->
  list_to_binary(Value).
