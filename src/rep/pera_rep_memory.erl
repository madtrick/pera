-module(pera_rep_memory).
-include("pera_hal_macros.hrl").

%% @hidden
%% @doc Module that returns a representation
%% for the memory of the system
%%
%% Currenty only the HAL format is supported.

-export([to_hal/2]).

%%========================================
%% API
%%========================================

%% @doc Returns a representation for the memory of the system
-spec to_hal(
  Memory  :: list({atom(), any()}),
  Options :: list({atom(), any()})
  ) -> binary().
to_hal(Memory, _) ->
  Resource = ?HAL_RESOURCE(
    [ ?HAL_REL(self, ?HAL_LINK(<<"/memory">>, [])) ],
    [],
    [?HAL_PROPERTY_OBJECT([?HAL_PROPERTY(Key, Value) || {Key, Value} <- Memory])]
  ),

  pera_hal_serializer:to_json(Resource).

