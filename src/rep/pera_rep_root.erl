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
    [ ?HAL_LINK(processes, <<"/processes">>, []) ],
    [],
    []
  ),

  pera_hal_serializer:to_json(Resource).

