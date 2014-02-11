-module(pera_processes).
-include("pera.hrl").

-export([all/0]).
-export([all_registered/0]).
-export([find/1]).
-export([find/2]).


-define(ALL_PROCESS_INFO_ITEMS, [binary, catchlevel, current_function, current_location, current_stacktrace, dictionary, error_handler, garbage_collection, group_leader, heap_size, initial_call, links, last_calls, memory, message_queue_len, messages, min_heap_size, min_bin_vheap_size, monitored_by, monitors, priority, reductions, registered_name, sequential_trace_token, stack_size, status, suspending, total_heap_size, trace, trap_exit]).

%% @doc Return all processes running in the current node
-spec all() -> list(pid()).
all() ->
  erlang:processes().

%% @doc Return all the registerd processes in the current node
-spec all_registered() -> list(atom()).
all_registered() ->
  erlang:registered().

%% @doc Return the process identified by the given pid
%% with info for all the possible items. See {@see erlang:process_info/2} for
%% a list of all the possible items.
-spec find(
  Pid :: pid()
  ) -> list({atom(), term()}).
find(PidString) ->
  find(PidString, ?ALL_PROCESS_INFO_ITEMS).

%% @doc Return the process identified by the given pid
%% with info for all given possible items. See {@see erlang:process_info/2} for
%% a list of all the items that can be requested.
-spec find(
  Pid   :: pid(),
  Items :: list(atom())
  ) -> list({atom(), term()}).
find(Pid, Items) ->
  lists:foldl(fun(Item, Acc) ->
        case erlang:process_info(Pid, Item) of
          undefined -> Acc;
          InfoTuple -> [InfoTuple | Acc]
        end
    end, [{pid, Pid}], Items).

