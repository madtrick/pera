-module(pera_modules).
-include("pera.hrl").

-export([all/0]).

%% @doc Return all the modules loaded in the system
-spec all() -> list({module(), string() | atom()}).
all() ->
  code:all_loaded().
