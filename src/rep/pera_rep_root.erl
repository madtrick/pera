-module(pera_rep_root).
-include("pera_hal_macros.hrl").

%% @hidden
%% @doc Module that returns a representation
%% for the root of the system
%%
%% Currenty only the HAL format is supported.

-export([to_hal/1]).

%%========================================
%% API
%%========================================

%% @doc Returns a representation for the root of the system
-spec to_hal(
  Options :: list({atom(), any()})
  ) -> binary().
to_hal(_) ->
  Resource = ?HAL_RESOURCE(
    [
      ?HAL_REL(self, ?HAL_LINK(<<"/">>, [])),
      ?HAL_REL(curies, [?HAL_LINK(<<"/relations/{rel}">>,[{templated, true}, {name, pera}])]),
      ?HAL_REL('pera:memory', ?HAL_LINK(<<"/memory">>, [])),
      ?HAL_REL('pera:modules', ?HAL_LINK(<<"/modules">>, [])),
      ?HAL_REL('pera:processes', ?HAL_LINK(<<"/processes">>, []))
    ],
    [],
    []
  ),

  pera_hal_serializer:to_json(Resource).

