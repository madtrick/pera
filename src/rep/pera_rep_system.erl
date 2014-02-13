-module(pera_rep_system).
-include("pera.hrl").
-include("pera_hal_macros.hrl").

%% @hidden
%% @doc Module that returns a representation
%% for the system info
%%
%% Currenty only the HAL format is supported.

-export([to_hal/2]).

%%========================================
%% API
%%========================================

%% @doc Returns a representation for the root of the system
-spec to_hal(
  Data    :: list({atom(), any()}),
  Options :: list({atom(), any()})
  ) -> binary().
to_hal(Data, _) ->
  Resource = ?HAL_RESOURCE(
    [
      ?HAL_REL(self, ?HAL_LINK(<<"/system">>, []))
    ],
    [],
    build_property_objects(Data)
  ),

  pera_hal_serializer:to_json(Resource).

-spec build_property_objects(
  Data :: list({atom(), any()})
  ) -> list(pera_hal_resource_property_object()).
build_property_objects(Data) ->
  [?HAL_PROPERTY_OBJECT([build_property_object(Property) || Property <- Data])].

-spec build_property_object(
  Property :: {atom(), any()}
  ) -> pera_hal_resource_property().
build_property_object({check_io, Items}) ->
  ?HAL_PROPERTY(check_io, ?HAL_PROPERTY_OBJECT([?HAL_PROPERTY(Name, Value) || {Name, Value} <- Items]));
build_property_object({c_compiler_used, {Name, {Major, Minor, Patch}}}) ->
  ?HAL_PROPERTY(c_compiler_used, ?HAL_PROPERTY_OBJECT(
      [
        ?HAL_PROPERTY(name, Name),
        ?HAL_PROPERTY(version, ?HAL_PROPERTY_OBJECT(
            [
              ?HAL_PROPERTY(major, Major),
              ?HAL_PROPERTY(minor, Minor),
              ?HAL_PROPERTY(patch, Patch)
            ]
          )
        )
      ])
  );
build_property_object({dist_ctrl, Controls}) ->
  case Controls of
    [] ->
      ?HAL_PROPERTY(dist_ctrl, []);
    Controls ->
      ?HAL_PROPERTY(dist_ctrl,
        [
          ?HAL_PROPERTY_OBJECT(
            [
              ?HAL_PROPERTY(
                erlang:atom_to_binary(Node, utf8),
                distribution_controller_entity_to_binary(Controller))
            || {Node, Controller} <- Controls]
        )]
      )
  end;
build_property_object({fullsweep_after, {_, After}}) ->
  ?HAL_PROPERTY(fullsweep_after, After);
build_property_object({garbage_collection, GCInfo}) ->
  ?HAL_PROPERTY(garbage_collection, ?HAL_PROPERTY_OBJECT([?HAL_PROPERTY(Key, Value) || {Key, Value} <- GCInfo]));
build_property_object({min_heap_size, {_, Size}}) ->
  ?HAL_PROPERTY(min_heap_size, Size);
build_property_object({min_bin_vheap_size, {_, Size}}) ->
  ?HAL_PROPERTY(min_bin_vheap_size, Size);
build_property_object({multi_scheduling_blockers, Blockers}) ->
  ?HAL_PROPERTY(multi_scheduling_blockers, [pera_utils:pid_to_binary(Blocker) || Blocker <- Blockers]);
build_property_object({scheduler_bindings, Bindings}) ->
  ?HAL_PROPERTY(scheduler_bindings, tuple_to_list(Bindings));
build_property_object({Key, Value}) ->
  case io_lib:printable_latin1_list(Value) of
    true -> ?HAL_PROPERTY(Key, list_to_binary(Value));
    false-> ?HAL_PROPERTY(Key, Value)
  end.
%build_property_object({Key, Value}) ->
%  ?HAL_PROPERTY(Key, Value).

%%========================================
%% Internal
%%========================================
-spec distribution_controller_entity_to_binary(
  Controller :: pid() | port()
  ) -> binary().
distribution_controller_entity_to_binary(Controller) when is_pid(Controller) ->
  pera_utils:pid_to_binary(Controller);
distribution_controller_entity_to_binary(Controller) ->
  erlang:list_to_binary(erlang:port_to_list(Controller)).
