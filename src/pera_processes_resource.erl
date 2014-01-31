-module(pera_processes_resource).

-export([init/1]).
-export([to_json/2]).
-export([content_types_provided/2]).

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
to_json(ReqData, State) ->
  {pera_processes:to_json(pera_processes:all()), ReqData, State}.

