-module(pera_hal_link_data_tests).
-include_lib("eunit/include/eunit.hrl").

hal_link_test() ->
  Rel     = "self",
  HREF    = "/resource",
  Options = [],
  Link    = pera_hal_link_data:new(Rel, HREF, Options),

  ?assert(pera_hal_link_data:get(rel, Link) == {ok, Rel}),
  ?assert(pera_hal_link_data:get(href, Link) == {ok, HREF}).

hal_link_with_options_tests() ->
  Rel  = "self",
  HREF = "/resource",
  Options = [{name, "link"}],
  Link = pera_hal_link_data:new(Rel, HREF, Options),

  ?assert(pera_hal_link_data:get(rel, Link) == {ok, Rel}),
  ?assert(pera_hal_link_data:get(href, Link) == {ok, HREF}),
  ?assert(pera_hal_link_data:get(name, Link) == {ok, "link"}),
  ?assert(pera_hal_link_data:get(templated, Link) == {ok, undefined}),
  ?assert(pera_hal_link_data:get(type, Link) == {ok, undefined}),
  ?assert(pera_hal_link_data:get(deprecation, Link) == {ok, undefined}),
  ?assert(pera_hal_link_data:get(title, Link) == {ok, undefined}),
  ?assert(pera_hal_link_data:get(profile, Link) == {ok, undefined}),
  ?assert(pera_hal_link_data:get(hreflang, Link) == {ok, undefined}).
