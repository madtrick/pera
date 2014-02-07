-module(pera_rep_processes).

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
  Resource = pera_hal:resource(
    [pera_hal:link(<<"self">>, <<"/processes">>, [])],
    [{<<"processes">>, [
          pera_hal:resource(
            [pera_hal:link(<<"self">>, <<"/processes/", (pera_utils:pid_to_binary(Process))/binary>>, [])],
            [],
            [{<<"pid">>, pera_utils:pid_to_binary(Process)}]
          )
          || Process <- Data]
      }],
    [{<<"total">>, erlang:length(Data)}]
  ),

  pera_hal_serializer:to_json(Resource).

