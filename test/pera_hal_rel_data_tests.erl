-module(pera_hal_rel_data_tests).
-include_lib("eunit/include/eunit.hrl").

hal_link_test() ->
  Name  = "self",
  Links = links,
  Rel   = pera_hal_rel_data:new(Name, Links),

  ?assert(pera_hal_rel_data:get(name, Rel) == {ok, Name}),
  ?assert(pera_hal_rel_data:get(links, Rel) == {ok, Links}).
