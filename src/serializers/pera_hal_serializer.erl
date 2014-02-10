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
  {ok, Rels}      = pera_hal_resource_data:get(rels, Resource),
  {ok, Embedded}   = pera_hal_resource_data:get(embedded, Resource),
  {ok, Properties} = pera_hal_resource_data:get(properties, Resource),

  pera_json:object([
      {<<"_links">>, resource_rels_to_json_object(Rels)} |
        [  {<<"_embedded">>, resource_embeddeds_to_json_object(Embedded)} |
          resource_property_objects_to_json_properties(Properties)
        ]
      ]).

-spec resource_rels_to_json_object(
  Rels :: list(pera_hal_rel())
  ) -> pera_json:json_object().
resource_rels_to_json_object([]) ->
  pera_json:object([]);
resource_rels_to_json_object(Rels) ->
  pera_json:object(resource_rels_to_json_object(Rels, [])).

-spec resource_rels_to_json_object(
  Rels :: list(pera_hal_rel()),
  Acc   :: list(pera_json:json_property())
  ) -> list(pera_json:property()).
resource_rels_to_json_object([], Acc) ->
  Acc;
resource_rels_to_json_object([Rel | Tail], Acc) ->
  resource_rels_to_json_object(Tail, [resource_rel_to_json(Rel) | Acc]).

-spec resource_rel_to_json(
  Link :: pera_hal_rel()
  ) -> pera_json:json_property().
resource_rel_to_json(Rel) ->
  {ok, Name}  = pera_hal_rel_data:get(name, Rel),
  {ok, Links} = pera_hal_rel_data:get(links, Rel),

  {Name, rel_links_to_json(Links)}.
  %{ok, Rel} = pera_hal_link_data:get(rel, Link),
  %{Rel, resource_link_attributes_to_json(Link)}.

-spec rel_links_to_json(
  Links :: pera_hal_link() | list(pera_hal_link())
) -> pera_json:json_value().
rel_links_to_json(Links) when is_list(Links) ->
  pera_json:array([rel_link_to_json_object(Link) || Link <- Links]);
rel_links_to_json(Link) ->
  rel_link_to_json_object(Link).

-spec rel_link_to_json_object(
  Link :: pera_hal_link()
  ) -> pera_json:json_object().
rel_link_to_json_object(Link) ->
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
  Acc       :: list({binary() | atom(), pera_json:object()})
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

-spec resource_property_objects_to_json_properties(
  PropertyObjects :: list(pera_hal_resource_property_object())
  ) -> list(pera_json:json_property()).
resource_property_objects_to_json_properties([]) ->
  [];
resource_property_objects_to_json_properties(PropertyObjects) ->
  resource_property_objects_to_json_properties(PropertyObjects, []).

-spec resource_property_objects_to_json_properties(
  PropertyObjects :: list(pera_hal_resource_property_object()),
  Acc             :: list(pera_json:json_property())
  ) -> list(pera_json:json_property()).
resource_property_objects_to_json_properties([], Acc) ->
  lists:flatten(Acc);
resource_property_objects_to_json_properties([Object | Tail], Acc) ->
  resource_property_objects_to_json_properties(Tail, [resource_property_object_to_json_properties(Object) | Acc]).

-spec resource_property_object_to_json_properties(
  Properties :: pera_hal_resource_property_object()
  ) -> list(pera_json:json_property()).
resource_property_object_to_json_properties({object, Properties}) ->
  resource_object_properties_to_json_properties(Properties).

-spec resource_object_properties_to_json_properties(
  Properties :: list(pera_hal_resource_property())
) -> list(pera_json:json_property()).
resource_object_properties_to_json_properties([]) ->
  [];
resource_object_properties_to_json_properties(Properties) ->
  resource_object_properties_to_json_properties(Properties, []).

-spec resource_object_properties_to_json_properties(
  Properties :: list(pera_hal_resource_property()),
  Acc        :: list(pera_json:json_property())
  ) -> list(pera_json:json_property()).
resource_object_properties_to_json_properties([], Acc) ->
  Acc;
resource_object_properties_to_json_properties([Property | Tail], Acc) ->
  resource_object_properties_to_json_properties(Tail, [resource_property_to_json_property(Property) | Acc]).

-spec resource_property_to_json_property(
  Property :: pera_hal_resource_property()
  ) -> pera_json:json_property().
resource_property_to_json_property({Key, Value}) ->
  {Key, resource_property_value_to_json_value(Value)}.

-spec resource_property_value_to_json_value(
  Value :: pera_hal_resource_property_value()
  ) -> pera_json:json_value().
resource_property_value_to_json_value(Value = {object, Properties}) ->
  pera_json:object(resource_property_object_to_json_properties(Value));
resource_property_value_to_json_value(Value) when is_list(Value) ->
  pera_json:array([resource_property_value_to_json_value(X) || X <- Value]);
resource_property_value_to_json_value(Value) ->
  Value.
