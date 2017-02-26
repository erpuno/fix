-module(fix_acceptor).
-include_lib("fix_macros.hrl").
-export([start/1, server/1]).

start(Dict)  -> ?DBG("start() ~w",[Dict]), {ok,spawn_link(?MODULE,server, [Dict])}.
server(Dict) -> ?DBG("server() ~w",[Dict]), Port = proplists:get_value(port, Dict, 9000),
                ?DBG("listing the port[~w]",[Port]),
                {ok, LSock} = gen_tcp:listen(Port, [binary,{packet,0}, {active, false}]),
                wait_connection(LSock), ok.

wait_connection(ListenSocket)->
    ?DBG("accepter:wait_connection() ~w", [ListenSocket]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    get_request(Socket).

get_request(Socket) ->
    case gen_tcp:recv(Socket, 0) of
         {ok, Data} -> ?DBG("incoming: ~w", [Data]),
                       Msg = fix_parser:parseFromFixToErl(Data),
                       fix_server:incoming_message(Socket, Msg),
                       get_request(Socket);
    {error, closed} -> ?DBG("socket[~w] is closed", [Socket]), ok end.