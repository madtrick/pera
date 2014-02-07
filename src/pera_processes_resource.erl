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
    Pid ->
      Process = pera_processes:find(http_uri:decode(Pid)),
      Response = pera_rep_process:to_hal(Process, []),
      {[{"application/hal+json", to_hal_json}], Req, Response}
  end.

%%========================================
%% Resource functions
%%========================================
to_hal_json(Req, Response) ->
  {Response, Req, Response}.
