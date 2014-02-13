-module(pera_system).

-export([find/0]).
-export([find/1]).

-define(ALL_INFO_ITEMS, [build_type , c_compiler_used , check_io , compat_rel , creation , debug_compiled , dist , dist_buf_busy_limit , dist_ctrl , driver_version , dynamic_trace , dynamic_trace_probes , elib_malloc , ets_limit , fullsweep_after , garbage_collection , heap_sizes , heap_type ,  kernel_poll ,  logical_processors, machine , min_heap_size , min_bin_vheap_size , modified_timing_level , multi_scheduling , multi_scheduling_blockers , otp_release , port_count , port_limit , process_count , process_limit ,  scheduler_bind_type , scheduler_bindings , scheduler_id , schedulers , smp_support , system_version , system_architecture , threads , thread_pool_size , trace_control_word , update_cpu_info , version , wordsize]).

%% @doc Returns info about the system
-spec find() -> list({atom(), any()}).
find() ->
  find(?ALL_INFO_ITEMS).

-spec find(
  Items :: list(atom())
  ) -> list({atom(), any()}).
find(Items) ->
  lists:foldl(fun(InfoItem, Acc) ->
    %%
    %% A try/catch is required as some items are not
    %% not available in all versions of Erlang
    %%
      try erlang:system_info(InfoItem) of
        Value ->
          [{InfoItem, Value} | Acc]
      catch
        error:badarg ->
          Acc
      end
  end, [], Items).
