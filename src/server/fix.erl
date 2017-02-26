-module(fix).
-behaviour(application).
-include_lib("fix_macros.hrl").
-compile(export_all).
-export([ start/2, stop/1, start_phase/3, prep_stop/1, config_change/3 ]).

test() -> eunit:test(fix).
gen() -> fix_gen:generate("priv/FIX42.xml").

start(Type, StartArgs) ->
    ?DBG("starting fix  type=[~w], args=[~w]", [Type, StartArgs]),
    case fix_supervisor:start(StartArgs) of
         {ok, Pid} -> {ok, Pid};
         Error -> Error end.

stop(State) -> ?DBG("stop fix. state=[~w]~n",[State]), ok.

start_phase(Phase, StartType, PhaseArgs) ->
    ?DBG("fix:start_phase() ~w, ~w, ~w", [Phase, StartType, PhaseArgs]), ok.

prep_stop(State) ->
    ?DBG("fix:pre_stop() ~w", [State]), State.

config_change(Changed, New, Removed) ->
    ?DBG("fix:config_change(Changed=[~w], New=[~w], Removed=[~w])", [Changed, New, Removed]), ok.
