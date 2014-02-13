-module(pera_system_resource).

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
  case wrq:get_qs_value("items", Req) of
    undefined ->
      Response = pera_rep_system:to_hal(pera_system:find(), []),
      {[{"application/hal+json", to_hal_json}], Req, Response};
    ItemsQuery->
      Items = query_list_values_to_atom_list(ItemsQuery),
      Response = pera_rep_system:to_hal(pera_system:find(Items), []),
      {[{"application/hal+json", to_hal_json}], Req, Response}
  end.


%%========================================
%% Resource functions
%%========================================
to_hal_json(Req, Response) ->
  {Response, Req, Response}.
%%
%%========================================
%% Helpers
%%========================================
-spec query_list_values_to_atom_list(
  Query :: string()
  ) -> list(atom()).
query_list_values_to_atom_list(Query) ->
  lists:map((fun(X) -> list_to_atom(X) end), string:tokens(Query, ",")).
