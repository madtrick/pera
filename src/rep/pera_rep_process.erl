-module(pera_rep_process).
-include("pera.hrl").
-include("pera_hal_macros.hrl").

%% @hidden
%% @doc Module that returns the representation of the
%% info that describes a process.
%%
%% Currenty only the HAL format is supported.

-export([to_hal/2]).

-define(PROPERTY_ITEMS, [catch_level, current_function, current_location, current_stacktrace, error_handler, heap_size, garbage_collection, initial_call, last_calls, links, memory, message_queue_len, min_heap_size, min_bin_vheap_size, monitored_by, monitors, priority, reductions, registered_name, stack_size, status, suspending, total_heap_size, trace, trap_exit]).

%%========================================
%% API
%%========================================

%% @doc Returns a representation of the info that describes a process
-spec to_hal(
  Data    :: list({atom(), term()}),
  Options :: list({atom(), any()})
  ) -> binary().
to_hal(Data, _) ->
  Pid = proplists:get_value(pid, Data),
  {Properties, _} = get_properties_from_process(Data),
  Resource = pera_hal:resource(
    build_links(Pid),
    [],
    build_property_objects(Properties)
  ),

  pera_hal_serializer:to_json(Resource).

%%========================================
%% Internal
%%========================================

-spec get_properties_from_process(
  Process :: list({atom(), term()})
) -> {list({atom(), term()}), list({atom(), term()})}.
get_properties_from_process(Process) ->
  {Properties, Remainder} = proplists:split(Process, ?PROPERTY_ITEMS),
  {lists:flatten(Properties), Remainder}.

-spec build_links(
  Pid :: pid()
  ) -> list(pera_hal_link()).
build_links(Pid) ->
  [
    ?HAL_REL(self, ?HAL_LINK(<<"/processes/", (pera_utils:pid_to_binary(Pid))/binary>>, [])),
    ?HAL_REL(curies, [?HAL_LINK(<<"/relations/{rel}.html">>,[{templated, true}, {name, pera}])]),
    ?HAL_REL('pera:modules', ?HAL_LINK(<<"/modules">>, [])),
    ?HAL_REL('pera:module', ?HAL_LINK(<<"/modules/{module}">>, [{templated, true}]))
  ].

-spec build_property_objects(
  Data :: list({atom(), term()})
  ) -> list(pera_hal_resource_property_object()).
build_property_objects(Data) ->
  [?HAL_PROPERTY_OBJECT([Property || Property <- ([build_property(Term) || Term <- Data])])].

-spec build_property(
  Term :: {atom(), term()}
  ) -> pera_hal_resource_property().
build_property({backtrace, Backtrace}) ->
  ?HAL_PROPERTY(backtrace, erlang:binary_to_list(Backtrace));
build_property({current_function, {Module, Function, Arity}}) ->
  ?HAL_PROPERTY(current_function, stack_item_info_to_property({Module, Function, Arity, []}));
build_property({current_location, LocationInfo}) ->
  ?HAL_PROPERTY(current_location, stack_item_info_to_property(LocationInfo));
build_property({current_stacktrace, StackItems}) ->
  ?HAL_PROPERTY(current_stacktrace, [stack_item_info_to_property(StackItem) || StackItem <- StackItems]);
build_property({initial_call, {Module, Function, Arity}}) ->
  ?HAL_PROPERTY(initial_call, stack_item_info_to_property({Module, Function, Arity, []}));
build_property({last_calls, false}) ->
  ?HAL_PROPERTY(last_calls, false);
build_property({last_calls, Calls}) ->
  ?HAL_PROPERTY(last_calls, [stack_item_info_to_property({M, F, A, []}) || {M, F, A} <- Calls]);
build_property({links, Links}) ->
  ?HAL_PROPERTY(links, [pera_utils:pid_to_binary(Link) || Link <- Links]);
build_property({monitored_by, Pids}) ->
  ?HAL_PROPERTY(monitored_by, [pera_utils:pid_to_binary(Pid) || Pid <- Pids]);
build_property({monitors, Monitors}) ->
  ?HAL_PROPERTY(monitors, [pera_utils:pid_to_binary(Monitor) || Monitor <- Monitors]);
build_property({garbage_collection, GCInfo}) ->
  ?HAL_PROPERTY(garbage_collection, ?HAL_PROPERTY_OBJECT([?HAL_PROPERTY(Key, Value) || {Key, Value} <- GCInfo]));
build_property({group_leader, GroupLeader}) ->
  ?HAL_PROPERTY(group_leader, pera_utils:pid_to_binary(GroupLeader));
build_property({suspending, SuspendingList}) ->
  Suspending = [
    ?HAL_PROPERTY_OBJECT(
      [
        ?HAL_PROPERTY(suspendee, Suspendee),
        ?HAL_PROPERTY(active_suspend_count, ActiveSuspendCount),
        ?HAL_PROPERTY(outstanding_suspend_count, OutStandingSuspendCount)
      ]
    )
    || {Suspendee, ActiveSuspendCount, OutStandingSuspendCount} <- SuspendingList],

  ?HAL_PROPERTY(suspending, Suspending);
build_property({Key, Value}) ->
  ?HAL_PROPERTY(Key, Value).

-spec stack_item_info_to_property(
  StackItemInfo :: {module(), atom(), arity() | [term()], [{file, string()} | {line, pos_integer()}] | []}
) -> pera_hal_resource_property_object().
stack_item_info_to_property({Module, Function, Arity, Location}) ->
  BaseProperties = [ ?HAL_PROPERTY(module, Module), ?HAL_PROPERTY(function, Function), ?HAL_PROPERTY(arity, Arity) ],
  Properties     = case Location of
    [] ->
     BaseProperties;
    _  ->
      [
        ?HAL_PROPERTY(file, list_to_binary(proplists:get_value(file, Location))),
        ?HAL_PROPERTY(line, proplists:get_value(line, Location))
        | BaseProperties
      ]
  end,

  ?HAL_PROPERTY_OBJECT(Properties).
