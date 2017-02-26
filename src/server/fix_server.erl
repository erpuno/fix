-module(fix_server).
-behaviour(gen_server).
-include_lib("fix_macros.hrl").
-export([start/0, incoming_message/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

start() -> ?DBG("starting fix server",[]), gen_server:start_link({local, ?MODULE}, ?MODULE,[],[]).
incoming_message(Socket, Data) -> gen_server:cast(?MODULE, {incoming,Socket,Data}).
init([]) -> ?DBG("fix_server:init()",[]), {ok, #state{}}.
handle_call(Request, From, State) ->
    ?DBG("fix_server:handle_call() request=~w, from=~w, state=~w~n",[Request,From,State]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    ?DBG("server:handle_cast() msg=~w, state=~w",[Msg,State]),
    case Msg of
         {incoming, Socket, FixMsg} -> ?DBG("incoming message ~w",[FixMsg]) end,
    {noreply, State}.

handle_info(Info, State) -> ?DBG("handle_info() info=~w, state=~w~n",[Info,State]), {noreply, State}.
terminate(Reason, State) -> ?DBG("fix_server:terminate() reason=~w, state=~w~n",[Reason,State]), ok.
code_change(OldVsn, State, Extra) -> ?DBG("fix_server:code_change() oldVsg=~w, state=~w, extra=~w~n",[OldVsn, State, Extra]), {ok, State}.
