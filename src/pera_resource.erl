-module(pera_resource).

-export([content_types_provided/2]).
-export([init/1]).
-export([to_hal_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

%%========================================
%% Webmachine resource functions
%%========================================
init([]) -> {ok, undefined}.

content_types_provided(Req, Context) ->
  Response = pera_rep_root:to_hal([]),
  {[{"application/hal+json", to_hal_json}], Req, Response}.

%%========================================
%% Resource functions
%%========================================
to_hal_json(Req, Response) ->
  {Response, Req, Response}.
