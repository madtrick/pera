-module(pera_hal_serializer).
-include("pera.hrl").

%% @hidden
%% @doc Module that transforms a HAL resource to  different representation formats.
%%
%% Currently only JSON is supported

-export([to_json/1]).

-define(RESOURCE_LINK_ATTRIBUTES, [href, templated, type, deprecation, name, profile, title, hreflang]).

%%========================================
%% API
%%========================================
-spec to_json(
  Resource :: pera_hal_resource()
  ) -> binary().
to_json(Resource) ->
  pera_json:to_json(resource_to_json_object(Resource)).

%%========================================
%% INTERNAL
%%========================================
-spec resource_to_json_object(
  Resource :: pera_hal_resource()
  ) -> pera_json:object().
resource_to_json_object(Resource) ->
  {ok, Links}      = pera_hal_resource_data:get(links, Resource),
  {ok, Embedded}   = pera_hal_resource_data:get(embedded, Resource),
  {ok, Properties} = pera_hal_resource_data:get(properties, Resource),

  pera_json:object([{<<"_links">>, resource_links_to_json(Links)} | [  {<<"_embedded">>, resource_embeddeds_to_json_object(Embedded)} | Properties ]]).

-spec resource_links_to_json(
  Links :: list(pera_hal_link())
  ) -> pera_json:json_object().
resource_links_to_json([]) ->
  pera_json:object([]);
resource_links_to_json(Links) ->
  pera_json:object(resource_links_to_json(Links, [])).

-spec resource_links_to_json(
  Links :: list(pera_hal_link()),
  Acc   :: list(pera_json:json_property())
  ) -> list(pera_json:property()).
resource_links_to_json([], Acc) ->
  Acc;
resource_links_to_json([Link | Tail], Acc) ->
  resource_links_to_json(Tail, [resource_link_to_json(Link) | Acc]).

-spec resource_link_to_json(
  Link :: pera_hal_link()
  ) -> pera_json:json_property().
resource_link_to_json(Link) ->
  {ok, Rel} = pera_hal_link_data:get(rel, Link),
  {Rel, resource_link_attributes_to_json(Link)}.

-spec resource_link_attributes_to_json(
  Link :: pera_hal_link()
  ) -> pera_json:json_object().
resource_link_attributes_to_json(Link) ->
  pera_json:object(
    lists:foldl(
      fun(AttributeName, Acc) ->
          case pera_hal_link_data:get(AttributeName, Link) of
            {ok, undefined} -> Acc;
            {ok, Value}     -> [{AttributeName, Value} | Acc];
            _               -> Acc
          end
    end, [],  ?RESOURCE_LINK_ATTRIBUTES)
).

-spec resource_embeddeds_to_json_object(
  Embeddeds :: list(pera_hal_embedded())
  ) -> pera_json:json_object().
resource_embeddeds_to_json_object([]) ->
  pera_json:object([]);
resource_embeddeds_to_json_object(Embeddeds)->
  pera_json:object(resource_embeddeds_to_json_object(Embeddeds, [])).

-spec resource_embeddeds_to_json_object(
  Embeddeds :: list(pera_hal_embedded()),
  Acc       :: list({binary(), pera_json:object()})
  ) -> list(pera_json:json_property()).
resource_embeddeds_to_json_object([], Acc) ->
  Acc;
resource_embeddeds_to_json_object([Embedded | Tail], Acc) ->
  resource_embeddeds_to_json_object(Tail, [resource_embedded_to_json_object(Embedded) | Acc]).

-spec resource_embedded_to_json_object(
  Embedded :: pera_hal_embedded()
  ) -> pera_json:json_property().
resource_embedded_to_json_object(Embedded) ->
  {Rel, Resource} = Embedded,
  case Resource of
    Resources when is_list(Resources) ->
      {Rel, pera_json:array([resource_to_json_object(R) || R <- Resources])};
    Resource ->
      {Rel, resource_to_json_object(Resource)}
  end.
