-module(pera_utils).

-export([pid_to_binary/1]).

-spec pid_to_binary(
  Pid :: pid()
  ) -> binary().
pid_to_binary(Pid) ->
  list_to_binary(pid_to_list(Pid)).
