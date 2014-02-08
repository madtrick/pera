-module(pera_rep_processes).
-include("pera_hal_macros.hrl").

%% @hidden
%% @doc Module that returns representations of a collection
%% of processes.
%%
%% Currenty only the HAL format is supported.

-export([to_hal/2]).

%% @doc Return a representation of the processes in the systems
-spec to_hal(
  Data    :: list(pid()),
  Options :: list({atom(), any()})
  ) -> binary().
to_hal(Data, _) ->
  Resource = ?HAL_RESOURCE(
      [?HAL_LINK(self, <<"/processes">>, [])],
      [?HAL_EMBEDDED(processes, [
            ?HAL_RESOURCE(
              [?HAL_LINK(self, <<"/processes/", (pera_utils:pid_to_binary(Process))/binary>>, [])],
              [],
              [?HAL_PROPERTY_OBJECT([{pid, pera_utils:pid_to_binary(Process)}])]
            )
          || Process <- Data])
      ],
      [?HAL_PROPERTY_OBJECT([{total, erlang:length(Data)}])]
    ),

  pera_hal_serializer:to_json(Resource).

