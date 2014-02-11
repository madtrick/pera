-module(pera_processes_resource).

-export([init/1]).
-export([to_hal_json/2]).
-export([content_types_provided/2]).

-include("pera.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%========================================
%% Webmachine resource functions
%%========================================
init([]) -> {ok, undefined}.

content_types_provided(Req, Context) ->
  case wrq:path_info(pid, Req) of
    undefined ->
      Processes = case wrq:get_qs_value("registered", Req) of
        "true" -> pera_processes:all_registered();
        _      -> pera_processes:all()
      end,
      Response = pera_rep_processes:to_hal(Processes, []),
      {[{"application/hal+json", to_hal_json}], Req, Response};
    EncodedPidValue ->
      Pid = process_pid_from_string_to_pid(http_uri:decode(EncodedPidValue)),
      Items = case wrq:get_qs_value("items", Req) of
        undefined -> [];
        ItemsQuery-> query_list_values_to_atom_list(ItemsQuery)
      end,

      Process = pera_processes:find(Pid, Items),
      Response = pera_rep_process:to_hal(Process, []),
      {[{"application/hal+json", to_hal_json}], Req, Response}
  end.

%%========================================
%% Resource functions
%%========================================
to_hal_json(Req, Response) ->
  {Response, Req, Response}.


%%========================================
%% Helpers
%%========================================
-spec query_list_values_to_atom_list(
  Query :: string()
  ) -> list(atom()).
query_list_values_to_atom_list(Query) ->
  lists:map((fun(X) -> list_to_atom(X) end), string:tokens(Query, ",")).

-spec process_pid_from_string_to_pid(
  String :: string()
  ) -> pid().
process_pid_from_string_to_pid(String) ->
  list_to_pid(String).
