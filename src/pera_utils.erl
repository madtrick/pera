-module(pera_utils).

-export([pid_to_binary/1]).
-export([urlencode_pid/1]).

-spec pid_to_binary(
  Pid :: pid()
  ) -> binary().
pid_to_binary(Pid) ->
  list_to_binary(pid_to_list(Pid)).

-spec urlencode_pid(
  Pid :: pid()
) -> string().
urlencode_pid(Pid) ->
  http_uri:encode(pid_to_list(Pid)).
