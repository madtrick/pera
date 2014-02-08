-module(pera_modules_resource).

-export([init/1]).
-export([to_hal_json/2]).
-export([content_types_provided/2]).

-include("pera.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%========================================
%% Webmachine resource functions
%%========================================
init([]) -> {ok, undefined}.

content_types_provided(Req, _) ->
  Response = pera_rep_modules:to_hal(pera_modules:all(), []),
  {[{"application/hal+json", to_hal_json}], Req, Response}.

%%========================================
%% Resource functions
%%========================================
to_hal_json(Req, Response) ->
  {Response, Req, Response}.
