-module(pera_processes_tests).
-include_lib("eunit/include/eunit.hrl").

returns_only_the_requested_items_test() ->
  % Assume that there's always a '<0.0.0>' pid
  Process = pera_processes:find(list_to_pid("<0.0.0>"), [heap_size, links]),

  % Process length is 3 because {pid, Pid} is 
  % included in the Process info and we use 
  % '=<' because there are attributes
  % that are not always available for every process
  ?assert(length(Process) =< 3),

  ?assert(proplists:is_defined(heap_size, Process)),
  ?assert(proplists:is_defined(links, Process)).
