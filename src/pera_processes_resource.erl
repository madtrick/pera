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
  {[{"application/hal+json", to_hal_json}], Req, Context}.

%%========================================
%% Resource functions
%%========================================
to_hal_json(Req, State) ->
  Response = case wrq:get_qs_value("registered", Req) of
    "true" -> pera_processes:all_registered();
    _      -> pera_processes:all()
  end,
  {pera_rep_processes:to_hal(Response, []), Req, State}.
