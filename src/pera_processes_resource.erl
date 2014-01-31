-module(pera_processes_resource).

-export([init/1]).
-export([to_json/2]).
-export([content_types_provided/2]).

-include("pera.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%========================================
%% Webmachine resource functions
%%========================================
init([]) -> {ok, undefined}.

content_types_provided(Req, Context) ->
  {[{"application/json", to_json}], Req, Context}.

%%========================================
%% Resource functions
%%========================================
to_json(Req, State) ->
  Response = case wrq:get_qs_value("registered", Req) of
    "true" -> pera_processes:all_registered();
    _      -> pera_processes:all()
  end,
  {(Response#pera_response.serializer):to_json(Response#pera_response.raw), Req, State}.
