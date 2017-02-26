-module(fix_encoder).
-export([encode/1]).
-define(SOH,[1]).

-spec encode(list(tuple()))->list().

encode([])->
    [];
encode([{beginString, Value} | REST]) ->
    lists:append("8=",Value,?SOH, encode(REST));
encode([{bodyLength, Value} | REST]) ->
    lists:append("9=",Value,?SOH, encode(REST));
encode([{msgType, Value} | REST]) ->
    lists:append("35=",Value,?SOH, encode(REST));
encode([{senderCompId, Value} | REST]) ->
    lists:append("49=", Value, ?SOH, encode(REST));
encode([{targetCompId, Value} | REST]) ->
    lists:append("56=", Value, ?SOH, encode(REST));
encode([{msgSeqNum,Value} | REST])->
    lists:append("34=", Value, ?SOH, encode(REST));
encode([{sendingTime,Value} | REST]) ->
    lists:append("52=", Value, ?SOH, encode(REST)).

