-module(pera_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


start(_Type, _StartArgs) ->
    pera_sup:start_link().

stop(_State) ->
    ok.
