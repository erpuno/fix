-module(fix).
-behaviour(application).
-include_lib("fix_macros.hrl").
-compile(export_all).
-export([ start/2, stop/1 ]).

test()      -> eunit:test(fix).
gen()       -> fix_gen:generate("priv/FIX42.xml").
stop(State) -> ok.
start(Type, StartArgs) ->
    case fix_supervisor:start(StartArgs) of
         {ok, Pid} -> {ok, Pid};
         Error -> Error end.
