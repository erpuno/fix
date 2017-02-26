-module(fix_supervisor).
-behaviour(supervisor).
-include_lib("fix_macros.hrl").
-compile(export_all).
-export([ init/1, start/1 ]).
-define(SERVER, ?MODULE).

start(Args) -> supervisor:start_link({local,?MODULE}, ?MODULE, [Args]).

init(StartArgs) ->
    ?DBG("init() ~w",[StartArgs]),
    init(acceptor, []).

init(acceptor, Args) ->
    FixSocket = {socket,{fix_acceptor,start, [Args]}, permanent, 2000, worker, [fix_acceptor]},
    FixServer = {server,{fix_server,start,[]}, permanent, 2000, worker,[fix_server]},
    {ok,{{one_for_one,100,100}, [FixServer, FixSocket]}};

init(initiator, Args) -> ok.

