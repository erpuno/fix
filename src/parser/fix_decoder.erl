%% @doc The module is responsible for decoding FIX string to Erlang inner structure
%% @author: Nikolay Volnov
-module(fix_decoder).

-export([decode/1]).

-define(SOH,[1]).


%% @doc decode a FIX string to list of tupl  
-spec decode(string() | binary()) -> [{atom(),any()}].

decode(MSG) when is_binary(MSG)->  decode(binary_to_list(MSG));
decode(MSG)-> parse_fields(string:tokens(MSG, ?SOH)).

%%@doc parse a list of TAG=VALUE tokens to a list of tuples

parse_fields([]) -> [];
parse_fields([FIELD | REST]) -> [TAG,VALUE] = string:tokens(FIELD, "="),
                                lists:append([convert({TAG,VALUE})], parse_fields(REST)).

%% @doc convert a FIX string pair TAG=VALUE to Erlang atom and Erlang term

convert({TAG,VALUE})  ->
    Tag2 = case TAG of
		"8" -> beginString;
		"9" -> bodyLength;
		"10" -> checkSum;
		"34" -> msgSeqNum;
		"35" -> msgType;
		"49" -> senderCompId;
		"50" -> senderSubId;
		"52" -> sendingTime;
		"56" -> targetCompId;
		"98" -> encryptMethod;
		"108" -> heartBtInt;
		Other -> list_to_atom(Other) %% @TODO unknown message
	end,
    {Tag2,convertValue(Tag2,VALUE)}.

%%====================================================
%% @doc convert a string value to int/date/...
%%===================================================
convertValue(msgType, Value)->
    case Value of
		 "A" -> logon;
		 "0" -> heartbeat;
		 Other -> list_to_atom(Other)
	     end;
convertValue(msgSeqNum, Value)->
    list_to_integer(Value);
convertValue(heartBtInt, Value) ->
    list_to_integer(Value);
convertValue(Tag,Value) ->
    Value.
