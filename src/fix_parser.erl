-module(fix_parser).

-include_lib("fix_messages.hrl").

-include_lib("fix_macros.hrl").

-export([encode/1, decode/1]).

-define(SOH,[1]).

encode(Fix) ->
ok.

decode(MSG) when is_binary(MSG) ->
    decode(binary_to_list(MSG));
decode(MSG) ->
    Tokens = string:tokens(MSG, ?SOH),
    Fun = fun(I) ->  string:tokens(I, "=") end,
    FieldList = lists:map(Fun, Tokens),
    [["8", BeginString], ["9", BodyLength], ["35", MsgType] | FieldList2] = FieldList,
    case MsgType of
        "m" -> decodeliststrikeprice(#liststrikeprice{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "l" -> decodebidresponse(#bidresponse{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "k" -> decodebidrequest(#bidrequest{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "j" -> decodebusinessmessagereject(#businessmessagereject{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "i" -> decodemassquote(#massquote{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "h" -> decodetradingsessionstatus(#tradingsessionstatus{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "g" -> decodetradingsessionstatusrequest(#tradingsessionstatusrequest{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "f" -> decodesecuritystatus(#securitystatus{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "e" -> decodesecuritystatusrequest(#securitystatusrequest{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "d" -> decodesecuritydefinition(#securitydefinition{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "c" -> decodesecuritydefinitionrequest(#securitydefinitionrequest{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "b" -> decodequoteacknowledgement(#quoteacknowledgement{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "a" -> decodequotestatusrequest(#quotestatusrequest{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "Z" -> decodequotecancel(#quotecancel{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "Y" -> decodemarketdatarequestreject(#marketdatarequestreject{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "X" -> decodemarketdataincrementalrefresh(#marketdataincrementalrefresh{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "W" -> decodemarketdatasnapshotfullrefresh(#marketdatasnapshotfullrefresh{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "V" -> decodemarketdatarequest(#marketdatarequest{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "T" -> decodesettlementinstructions(#settlementinstructions{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "S" -> decodequote(#quote{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "R" -> decodequoterequest(#quoterequest{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "Q" -> decodedontknowtrade(#dontknowtrade{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "P" -> decodeallocationack(#allocationack{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "N" -> decodeliststatus(#liststatus{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "M" -> decodeliststatusrequest(#liststatusrequest{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "L" -> decodelistexecute(#listexecute{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "K" -> decodelistcancelrequest(#listcancelrequest{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "J" -> decodeallocation(#allocation{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "H" -> decodeorderstatusrequest(#orderstatusrequest{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "G" -> decodeordercancelreplacerequest(#ordercancelreplacerequest{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "F" -> decodeordercancelrequest(#ordercancelrequest{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "E" -> decodeneworderlist(#neworderlist{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "D" -> decodenewordersingle(#newordersingle{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "C" -> decodeemail(#email{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "B" -> decodenews(#news{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "A" -> decodelogon(#logon{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "9" -> decodeordercancelreject(#ordercancelreject{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "8" -> decodeexecutionreport(#executionreport{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "7" -> decodeadvertisement(#advertisement{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "6" -> decodeindicationofinterest(#indicationofinterest{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "5" -> decodelogout(#logout{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "4" -> decodesequencereset(#sequencereset{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "3" -> decodereject(#reject{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "2" -> decoderesendrequest(#resendrequest{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "1" -> decodetestrequest(#testrequest{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        "0" -> decodeheartbeat(#heartbeat{header=#header{beginstring=BeginString, bodylength=BodyLength, msgtype = MsgType}, trailer=#trailer{}}, FieldList2)	;
        false -> false %% wrong format
    end. 

decodeheartbeat(Msg,[]) -> 
    Msg;
decodeheartbeat(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{onbehalfofsendingtime=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{lastmsgseqnumprocessed=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{messageencoding=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{xmldata=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{xmldatalen=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{origsendingtime=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{sendingtime=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{possresend=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{possdupflag=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{delivertolocationid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{delivertosubid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{onbehalfoflocationid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{onbehalfofsubid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{targetlocationid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{targetsubid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{senderlocationid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{sendersubid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{msgseqnum=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{securedata=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{securedatalen=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{delivertocompid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{onbehalfofcompid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{targetcompid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{sendercompid=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{msgtype=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{bodylength=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#heartbeat.header#header{beginstring=Value},
    decodeheartbeat(Msg#heartbeat{header=Header},Rest);
decodeheartbeat(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#heartbeat.trailer#trailer{checksum=Value},
    decodeheartbeat(Msg#heartbeat{trailer=Trailer},Rest);
decodeheartbeat(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#heartbeat.trailer#trailer{signature=Value},
    decodeheartbeat(Msg#heartbeat{trailer=Trailer},Rest);
decodeheartbeat(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#heartbeat.trailer#trailer{signaturelength=Value},
    decodeheartbeat(Msg#heartbeat{trailer=Trailer},Rest);
decodeheartbeat(Msg, [["112", Value] | Rest]) -> 
    decodeheartbeat(Msg#heartbeat{testreqid=Value}, Rest);
decodeheartbeat(Msg, [[_, Value] | Rest]) ->
    decodeheartbeat(Msg,Rest).

decodetestrequest(Msg,[]) -> 
    Msg;
decodetestrequest(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{onbehalfofsendingtime=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{lastmsgseqnumprocessed=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{messageencoding=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{xmldata=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{xmldatalen=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{origsendingtime=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{sendingtime=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{possresend=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{possdupflag=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{delivertolocationid=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{delivertosubid=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{onbehalfoflocationid=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{onbehalfofsubid=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{targetlocationid=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{targetsubid=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{senderlocationid=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{sendersubid=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{msgseqnum=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{securedata=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{securedatalen=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{delivertocompid=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{onbehalfofcompid=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{targetcompid=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{sendercompid=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{msgtype=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{bodylength=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#testrequest.header#header{beginstring=Value},
    decodetestrequest(Msg#testrequest{header=Header},Rest);
decodetestrequest(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#testrequest.trailer#trailer{checksum=Value},
    decodetestrequest(Msg#testrequest{trailer=Trailer},Rest);
decodetestrequest(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#testrequest.trailer#trailer{signature=Value},
    decodetestrequest(Msg#testrequest{trailer=Trailer},Rest);
decodetestrequest(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#testrequest.trailer#trailer{signaturelength=Value},
    decodetestrequest(Msg#testrequest{trailer=Trailer},Rest);
decodetestrequest(Msg, [["112", Value] | Rest]) -> 
    decodetestrequest(Msg#testrequest{testreqid=Value}, Rest);
decodetestrequest(Msg, [[_, Value] | Rest]) ->
    decodetestrequest(Msg,Rest).

decoderesendrequest(Msg,[]) -> 
    Msg;
decoderesendrequest(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{onbehalfofsendingtime=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{lastmsgseqnumprocessed=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{messageencoding=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{xmldata=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{xmldatalen=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{origsendingtime=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{sendingtime=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{possresend=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{possdupflag=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{delivertolocationid=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{delivertosubid=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{onbehalfoflocationid=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{onbehalfofsubid=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{targetlocationid=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{targetsubid=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{senderlocationid=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{sendersubid=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{msgseqnum=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{securedata=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{securedatalen=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{delivertocompid=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{onbehalfofcompid=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{targetcompid=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{sendercompid=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{msgtype=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{bodylength=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#resendrequest.header#header{beginstring=Value},
    decoderesendrequest(Msg#resendrequest{header=Header},Rest);
decoderesendrequest(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#resendrequest.trailer#trailer{checksum=Value},
    decoderesendrequest(Msg#resendrequest{trailer=Trailer},Rest);
decoderesendrequest(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#resendrequest.trailer#trailer{signature=Value},
    decoderesendrequest(Msg#resendrequest{trailer=Trailer},Rest);
decoderesendrequest(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#resendrequest.trailer#trailer{signaturelength=Value},
    decoderesendrequest(Msg#resendrequest{trailer=Trailer},Rest);
decoderesendrequest(Msg, [["16", Value] | Rest]) -> 
    decoderesendrequest(Msg#resendrequest{endseqno=Value}, Rest);
decoderesendrequest(Msg, [["7", Value] | Rest]) -> 
    decoderesendrequest(Msg#resendrequest{beginseqno=Value}, Rest);
decoderesendrequest(Msg, [[_, Value] | Rest]) ->
    decoderesendrequest(Msg,Rest).

decodereject(Msg,[]) -> 
    Msg;
decodereject(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#reject.header#header{onbehalfofsendingtime=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#reject.header#header{lastmsgseqnumprocessed=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#reject.header#header{messageencoding=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#reject.header#header{xmldata=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#reject.header#header{xmldatalen=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#reject.header#header{origsendingtime=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#reject.header#header{sendingtime=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#reject.header#header{possresend=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#reject.header#header{possdupflag=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#reject.header#header{delivertolocationid=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#reject.header#header{delivertosubid=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#reject.header#header{onbehalfoflocationid=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#reject.header#header{onbehalfofsubid=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#reject.header#header{targetlocationid=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#reject.header#header{targetsubid=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#reject.header#header{senderlocationid=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#reject.header#header{sendersubid=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#reject.header#header{msgseqnum=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#reject.header#header{securedata=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#reject.header#header{securedatalen=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#reject.header#header{delivertocompid=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#reject.header#header{onbehalfofcompid=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#reject.header#header{targetcompid=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#reject.header#header{sendercompid=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#reject.header#header{msgtype=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#reject.header#header{bodylength=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#reject.header#header{beginstring=Value},
    decodereject(Msg#reject{header=Header},Rest);
decodereject(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#reject.trailer#trailer{checksum=Value},
    decodereject(Msg#reject{trailer=Trailer},Rest);
decodereject(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#reject.trailer#trailer{signature=Value},
    decodereject(Msg#reject{trailer=Trailer},Rest);
decodereject(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#reject.trailer#trailer{signaturelength=Value},
    decodereject(Msg#reject{trailer=Trailer},Rest);
decodereject(Msg, [["355", Value] | Rest]) -> 
    decodereject(Msg#reject{encodedtext=Value}, Rest);
decodereject(Msg, [["354", Value] | Rest]) -> 
    decodereject(Msg#reject{encodedtextlen=Value}, Rest);
decodereject(Msg, [["58", Value] | Rest]) -> 
    decodereject(Msg#reject{text=Value}, Rest);
decodereject(Msg, [["373", Value] | Rest]) -> 
    decodereject(Msg#reject{sessionrejectreason=Value}, Rest);
decodereject(Msg, [["372", Value] | Rest]) -> 
    decodereject(Msg#reject{refmsgtype=Value}, Rest);
decodereject(Msg, [["371", Value] | Rest]) -> 
    decodereject(Msg#reject{reftagid=Value}, Rest);
decodereject(Msg, [["45", Value] | Rest]) -> 
    decodereject(Msg#reject{refseqnum=Value}, Rest);
decodereject(Msg, [[_, Value] | Rest]) ->
    decodereject(Msg,Rest).

decodesequencereset(Msg,[]) -> 
    Msg;
decodesequencereset(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{onbehalfofsendingtime=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{lastmsgseqnumprocessed=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{messageencoding=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{xmldata=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{xmldatalen=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{origsendingtime=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{sendingtime=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{possresend=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{possdupflag=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{delivertolocationid=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{delivertosubid=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{onbehalfoflocationid=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{onbehalfofsubid=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{targetlocationid=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{targetsubid=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{senderlocationid=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{sendersubid=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{msgseqnum=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{securedata=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{securedatalen=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{delivertocompid=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{onbehalfofcompid=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{targetcompid=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{sendercompid=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{msgtype=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{bodylength=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#sequencereset.header#header{beginstring=Value},
    decodesequencereset(Msg#sequencereset{header=Header},Rest);
decodesequencereset(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#sequencereset.trailer#trailer{checksum=Value},
    decodesequencereset(Msg#sequencereset{trailer=Trailer},Rest);
decodesequencereset(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#sequencereset.trailer#trailer{signature=Value},
    decodesequencereset(Msg#sequencereset{trailer=Trailer},Rest);
decodesequencereset(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#sequencereset.trailer#trailer{signaturelength=Value},
    decodesequencereset(Msg#sequencereset{trailer=Trailer},Rest);
decodesequencereset(Msg, [["36", Value] | Rest]) -> 
    decodesequencereset(Msg#sequencereset{newseqno=Value}, Rest);
decodesequencereset(Msg, [["123", Value] | Rest]) -> 
    decodesequencereset(Msg#sequencereset{gapfillflag=Value}, Rest);
decodesequencereset(Msg, [[_, Value] | Rest]) ->
    decodesequencereset(Msg,Rest).

decodelogout(Msg,[]) -> 
    Msg;
decodelogout(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#logout.header#header{onbehalfofsendingtime=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#logout.header#header{lastmsgseqnumprocessed=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#logout.header#header{messageencoding=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#logout.header#header{xmldata=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#logout.header#header{xmldatalen=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#logout.header#header{origsendingtime=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#logout.header#header{sendingtime=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#logout.header#header{possresend=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#logout.header#header{possdupflag=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#logout.header#header{delivertolocationid=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#logout.header#header{delivertosubid=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#logout.header#header{onbehalfoflocationid=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#logout.header#header{onbehalfofsubid=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#logout.header#header{targetlocationid=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#logout.header#header{targetsubid=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#logout.header#header{senderlocationid=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#logout.header#header{sendersubid=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#logout.header#header{msgseqnum=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#logout.header#header{securedata=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#logout.header#header{securedatalen=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#logout.header#header{delivertocompid=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#logout.header#header{onbehalfofcompid=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#logout.header#header{targetcompid=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#logout.header#header{sendercompid=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#logout.header#header{msgtype=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#logout.header#header{bodylength=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#logout.header#header{beginstring=Value},
    decodelogout(Msg#logout{header=Header},Rest);
decodelogout(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#logout.trailer#trailer{checksum=Value},
    decodelogout(Msg#logout{trailer=Trailer},Rest);
decodelogout(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#logout.trailer#trailer{signature=Value},
    decodelogout(Msg#logout{trailer=Trailer},Rest);
decodelogout(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#logout.trailer#trailer{signaturelength=Value},
    decodelogout(Msg#logout{trailer=Trailer},Rest);
decodelogout(Msg, [["355", Value] | Rest]) -> 
    decodelogout(Msg#logout{encodedtext=Value}, Rest);
decodelogout(Msg, [["354", Value] | Rest]) -> 
    decodelogout(Msg#logout{encodedtextlen=Value}, Rest);
decodelogout(Msg, [["58", Value] | Rest]) -> 
    decodelogout(Msg#logout{text=Value}, Rest);
decodelogout(Msg, [[_, Value] | Rest]) ->
    decodelogout(Msg,Rest).

decodeindicationofinterest(Msg,[]) -> 
    Msg;
decodeindicationofinterest(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{onbehalfofsendingtime=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{lastmsgseqnumprocessed=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{messageencoding=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{xmldata=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{xmldatalen=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{origsendingtime=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{sendingtime=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{possresend=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{possdupflag=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{delivertolocationid=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{delivertosubid=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{onbehalfoflocationid=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{onbehalfofsubid=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{targetlocationid=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{targetsubid=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{senderlocationid=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{sendersubid=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{msgseqnum=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{securedata=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{securedatalen=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{delivertocompid=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{onbehalfofcompid=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{targetcompid=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{sendercompid=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{msgtype=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{bodylength=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#indicationofinterest.header#header{beginstring=Value},
    decodeindicationofinterest(Msg#indicationofinterest{header=Header},Rest);
decodeindicationofinterest(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#indicationofinterest.trailer#trailer{checksum=Value},
    decodeindicationofinterest(Msg#indicationofinterest{trailer=Trailer},Rest);
decodeindicationofinterest(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#indicationofinterest.trailer#trailer{signature=Value},
    decodeindicationofinterest(Msg#indicationofinterest{trailer=Trailer},Rest);
decodeindicationofinterest(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#indicationofinterest.trailer#trailer{signaturelength=Value},
    decodeindicationofinterest(Msg#indicationofinterest{trailer=Trailer},Rest);
decodeindicationofinterest(Msg, [["219", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{benchmark=Value}, Rest);
decodeindicationofinterest(Msg, [["218", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{spreadtobenchmark=Value}, Rest);
decodeindicationofinterest(Msg, [["149", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{urllink=Value}, Rest);
decodeindicationofinterest(Msg, [["60", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{transacttime=Value}, Rest);
decodeindicationofinterest(Msg, [["355", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{encodedtext=Value}, Rest);
decodeindicationofinterest(Msg, [["354", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{encodedtextlen=Value}, Rest);
decodeindicationofinterest(Msg, [["58", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{text=Value}, Rest);
decodeindicationofinterest(Msg, [["130", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{ioinaturalflag=Value}, Rest);
decodeindicationofinterest(Msg, [["25", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{ioiqltyind=Value}, Rest);
decodeindicationofinterest(Msg, [["62", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{validuntiltime=Value}, Rest);
decodeindicationofinterest(Msg, [["15", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{currency=Value}, Rest);
decodeindicationofinterest(Msg, [["44", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{price=Value}, Rest);
decodeindicationofinterest(Msg, [["27", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{ioishares=Value}, Rest);
decodeindicationofinterest(Msg, [["54", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{side=Value}, Rest);
decodeindicationofinterest(Msg, [["351", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{encodedsecuritydesc=Value}, Rest);
decodeindicationofinterest(Msg, [["350", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{encodedsecuritydesclen=Value}, Rest);
decodeindicationofinterest(Msg, [["107", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{securitydesc=Value}, Rest);
decodeindicationofinterest(Msg, [["349", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{encodedissuer=Value}, Rest);
decodeindicationofinterest(Msg, [["348", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{encodedissuerlen=Value}, Rest);
decodeindicationofinterest(Msg, [["106", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{issuer=Value}, Rest);
decodeindicationofinterest(Msg, [["207", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{securityexchange=Value}, Rest);
decodeindicationofinterest(Msg, [["223", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{couponrate=Value}, Rest);
decodeindicationofinterest(Msg, [["231", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{contractmultiplier=Value}, Rest);
decodeindicationofinterest(Msg, [["206", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{optattribute=Value}, Rest);
decodeindicationofinterest(Msg, [["202", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{strikeprice=Value}, Rest);
decodeindicationofinterest(Msg, [["201", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{putorcall=Value}, Rest);
decodeindicationofinterest(Msg, [["205", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{maturityday=Value}, Rest);
decodeindicationofinterest(Msg, [["200", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{maturitymonthyear=Value}, Rest);
decodeindicationofinterest(Msg, [["167", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{securitytype=Value}, Rest);
decodeindicationofinterest(Msg, [["22", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{idsource=Value}, Rest);
decodeindicationofinterest(Msg, [["48", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{securityid=Value}, Rest);
decodeindicationofinterest(Msg, [["65", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{symbolsfx=Value}, Rest);
decodeindicationofinterest(Msg, [["55", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{symbol=Value}, Rest);
decodeindicationofinterest(Msg, [["26", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{ioirefid=Value}, Rest);
decodeindicationofinterest(Msg, [["28", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{ioitranstype=Value}, Rest);
decodeindicationofinterest(Msg, [["23", Value] | Rest]) -> 
    decodeindicationofinterest(Msg#indicationofinterest{ioiid=Value}, Rest);
decodeindicationofinterest(Msg, [[_, Value] | Rest]) ->
    decodeindicationofinterest(Msg,Rest).

decodeadvertisement(Msg,[]) -> 
    Msg;
decodeadvertisement(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{onbehalfofsendingtime=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{lastmsgseqnumprocessed=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{messageencoding=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{xmldata=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{xmldatalen=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{origsendingtime=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{sendingtime=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{possresend=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{possdupflag=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{delivertolocationid=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{delivertosubid=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{onbehalfoflocationid=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{onbehalfofsubid=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{targetlocationid=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{targetsubid=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{senderlocationid=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{sendersubid=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{msgseqnum=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{securedata=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{securedatalen=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{delivertocompid=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{onbehalfofcompid=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{targetcompid=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{sendercompid=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{msgtype=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{bodylength=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#advertisement.header#header{beginstring=Value},
    decodeadvertisement(Msg#advertisement{header=Header},Rest);
decodeadvertisement(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#advertisement.trailer#trailer{checksum=Value},
    decodeadvertisement(Msg#advertisement{trailer=Trailer},Rest);
decodeadvertisement(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#advertisement.trailer#trailer{signature=Value},
    decodeadvertisement(Msg#advertisement{trailer=Trailer},Rest);
decodeadvertisement(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#advertisement.trailer#trailer{signaturelength=Value},
    decodeadvertisement(Msg#advertisement{trailer=Trailer},Rest);
decodeadvertisement(Msg, [["336", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{tradingsessionid=Value}, Rest);
decodeadvertisement(Msg, [["30", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{lastmkt=Value}, Rest);
decodeadvertisement(Msg, [["149", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{urllink=Value}, Rest);
decodeadvertisement(Msg, [["355", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{encodedtext=Value}, Rest);
decodeadvertisement(Msg, [["354", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{encodedtextlen=Value}, Rest);
decodeadvertisement(Msg, [["58", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{text=Value}, Rest);
decodeadvertisement(Msg, [["60", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{transacttime=Value}, Rest);
decodeadvertisement(Msg, [["75", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{tradedate=Value}, Rest);
decodeadvertisement(Msg, [["15", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{currency=Value}, Rest);
decodeadvertisement(Msg, [["44", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{price=Value}, Rest);
decodeadvertisement(Msg, [["53", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{shares=Value}, Rest);
decodeadvertisement(Msg, [["4", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{advside=Value}, Rest);
decodeadvertisement(Msg, [["351", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{encodedsecuritydesc=Value}, Rest);
decodeadvertisement(Msg, [["350", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{encodedsecuritydesclen=Value}, Rest);
decodeadvertisement(Msg, [["107", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{securitydesc=Value}, Rest);
decodeadvertisement(Msg, [["349", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{encodedissuer=Value}, Rest);
decodeadvertisement(Msg, [["348", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{encodedissuerlen=Value}, Rest);
decodeadvertisement(Msg, [["106", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{issuer=Value}, Rest);
decodeadvertisement(Msg, [["207", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{securityexchange=Value}, Rest);
decodeadvertisement(Msg, [["223", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{couponrate=Value}, Rest);
decodeadvertisement(Msg, [["231", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{contractmultiplier=Value}, Rest);
decodeadvertisement(Msg, [["206", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{optattribute=Value}, Rest);
decodeadvertisement(Msg, [["202", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{strikeprice=Value}, Rest);
decodeadvertisement(Msg, [["201", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{putorcall=Value}, Rest);
decodeadvertisement(Msg, [["205", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{maturityday=Value}, Rest);
decodeadvertisement(Msg, [["200", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{maturitymonthyear=Value}, Rest);
decodeadvertisement(Msg, [["167", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{securitytype=Value}, Rest);
decodeadvertisement(Msg, [["22", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{idsource=Value}, Rest);
decodeadvertisement(Msg, [["48", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{securityid=Value}, Rest);
decodeadvertisement(Msg, [["65", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{symbolsfx=Value}, Rest);
decodeadvertisement(Msg, [["55", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{symbol=Value}, Rest);
decodeadvertisement(Msg, [["3", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{advrefid=Value}, Rest);
decodeadvertisement(Msg, [["5", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{advtranstype=Value}, Rest);
decodeadvertisement(Msg, [["2", Value] | Rest]) -> 
    decodeadvertisement(Msg#advertisement{advid=Value}, Rest);
decodeadvertisement(Msg, [[_, Value] | Rest]) ->
    decodeadvertisement(Msg,Rest).

decodeexecutionreport(Msg,[]) -> 
    Msg;
decodeexecutionreport(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{onbehalfofsendingtime=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{lastmsgseqnumprocessed=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{messageencoding=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{xmldata=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{xmldatalen=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{origsendingtime=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{sendingtime=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{possresend=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{possdupflag=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{delivertolocationid=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{delivertosubid=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{onbehalfoflocationid=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{onbehalfofsubid=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{targetlocationid=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{targetsubid=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{senderlocationid=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{sendersubid=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{msgseqnum=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{securedata=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{securedatalen=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{delivertocompid=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{onbehalfofcompid=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{targetcompid=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{sendercompid=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{msgtype=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{bodylength=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#executionreport.header#header{beginstring=Value},
    decodeexecutionreport(Msg#executionreport{header=Header},Rest);
decodeexecutionreport(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#executionreport.trailer#trailer{checksum=Value},
    decodeexecutionreport(Msg#executionreport{trailer=Trailer},Rest);
decodeexecutionreport(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#executionreport.trailer#trailer{signature=Value},
    decodeexecutionreport(Msg#executionreport{trailer=Trailer},Rest);
decodeexecutionreport(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#executionreport.trailer#trailer{signaturelength=Value},
    decodeexecutionreport(Msg#executionreport{trailer=Trailer},Rest);
decodeexecutionreport(Msg, [["442", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{multilegreportingtype=Value}, Rest);
decodeexecutionreport(Msg, [["440", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{clearingaccount=Value}, Rest);
decodeexecutionreport(Msg, [["439", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{clearingfirm=Value}, Rest);
decodeexecutionreport(Msg, [["192", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{orderqty2=Value}, Rest);
decodeexecutionreport(Msg, [["193", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{futsettdate2=Value}, Rest);
decodeexecutionreport(Msg, [["355", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{encodedtext=Value}, Rest);
decodeexecutionreport(Msg, [["354", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{encodedtextlen=Value}, Rest);
decodeexecutionreport(Msg, [["58", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{text=Value}, Rest);
decodeexecutionreport(Msg, [["210", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{maxshow=Value}, Rest);
decodeexecutionreport(Msg, [["77", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{openclose=Value}, Rest);
decodeexecutionreport(Msg, [["111", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{maxfloor=Value}, Rest);
decodeexecutionreport(Msg, [["110", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{minqty=Value}, Rest);
decodeexecutionreport(Msg, [["21", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{handlinst=Value}, Rest);
decodeexecutionreport(Msg, [["156", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{settlcurrfxratecalc=Value}, Rest);
decodeexecutionreport(Msg, [["155", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{settlcurrfxrate=Value}, Rest);
decodeexecutionreport(Msg, [["120", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{settlcurrency=Value}, Rest);
decodeexecutionreport(Msg, [["119", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{settlcurramt=Value}, Rest);
decodeexecutionreport(Msg, [["381", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{grosstradeamt=Value}, Rest);
decodeexecutionreport(Msg, [["13", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{commtype=Value}, Rest);
decodeexecutionreport(Msg, [["12", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{commission=Value}, Rest);
decodeexecutionreport(Msg, [["113", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{reporttoexch=Value}, Rest);
decodeexecutionreport(Msg, [["60", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{transacttime=Value}, Rest);
decodeexecutionreport(Msg, [["75", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{tradedate=Value}, Rest);
decodeexecutionreport(Msg, [["427", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{gtbookinginst=Value}, Rest);
decodeexecutionreport(Msg, [["426", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{dayavgpx=Value}, Rest);
decodeexecutionreport(Msg, [["425", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{daycumqty=Value}, Rest);
decodeexecutionreport(Msg, [["424", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{dayorderqty=Value}, Rest);
decodeexecutionreport(Msg, [["6", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{avgpx=Value}, Rest);
decodeexecutionreport(Msg, [["14", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{cumqty=Value}, Rest);
decodeexecutionreport(Msg, [["151", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{leavesqty=Value}, Rest);
decodeexecutionreport(Msg, [["29", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{lastcapacity=Value}, Rest);
decodeexecutionreport(Msg, [["336", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{tradingsessionid=Value}, Rest);
decodeexecutionreport(Msg, [["30", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{lastmkt=Value}, Rest);
decodeexecutionreport(Msg, [["195", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{lastforwardpoints=Value}, Rest);
decodeexecutionreport(Msg, [["194", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{lastspotrate=Value}, Rest);
decodeexecutionreport(Msg, [["31", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{lastpx=Value}, Rest);
decodeexecutionreport(Msg, [["32", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{lastshares=Value}, Rest);
decodeexecutionreport(Msg, [["47", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{rule80a=Value}, Rest);
decodeexecutionreport(Msg, [["18", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{execinst=Value}, Rest);
decodeexecutionreport(Msg, [["126", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{expiretime=Value}, Rest);
decodeexecutionreport(Msg, [["432", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{expiredate=Value}, Rest);
decodeexecutionreport(Msg, [["168", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{effectivetime=Value}, Rest);
decodeexecutionreport(Msg, [["59", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{timeinforce=Value}, Rest);
decodeexecutionreport(Msg, [["377", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{solicitedflag=Value}, Rest);
decodeexecutionreport(Msg, [["376", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{complianceid=Value}, Rest);
decodeexecutionreport(Msg, [["15", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{currency=Value}, Rest);
decodeexecutionreport(Msg, [["389", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{discretionoffset=Value}, Rest);
decodeexecutionreport(Msg, [["388", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{discretioninst=Value}, Rest);
decodeexecutionreport(Msg, [["211", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{pegdifference=Value}, Rest);
decodeexecutionreport(Msg, [["99", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{stoppx=Value}, Rest);
decodeexecutionreport(Msg, [["44", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{price=Value}, Rest);
decodeexecutionreport(Msg, [["40", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{ordtype=Value}, Rest);
decodeexecutionreport(Msg, [["152", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{cashorderqty=Value}, Rest);
decodeexecutionreport(Msg, [["38", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{orderqty=Value}, Rest);
decodeexecutionreport(Msg, [["54", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{side=Value}, Rest);
decodeexecutionreport(Msg, [["351", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{encodedsecuritydesc=Value}, Rest);
decodeexecutionreport(Msg, [["350", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{encodedsecuritydesclen=Value}, Rest);
decodeexecutionreport(Msg, [["107", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{securitydesc=Value}, Rest);
decodeexecutionreport(Msg, [["349", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{encodedissuer=Value}, Rest);
decodeexecutionreport(Msg, [["348", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{encodedissuerlen=Value}, Rest);
decodeexecutionreport(Msg, [["106", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{issuer=Value}, Rest);
decodeexecutionreport(Msg, [["207", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{securityexchange=Value}, Rest);
decodeexecutionreport(Msg, [["223", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{couponrate=Value}, Rest);
decodeexecutionreport(Msg, [["231", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{contractmultiplier=Value}, Rest);
decodeexecutionreport(Msg, [["206", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{optattribute=Value}, Rest);
decodeexecutionreport(Msg, [["202", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{strikeprice=Value}, Rest);
decodeexecutionreport(Msg, [["201", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{putorcall=Value}, Rest);
decodeexecutionreport(Msg, [["205", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{maturityday=Value}, Rest);
decodeexecutionreport(Msg, [["200", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{maturitymonthyear=Value}, Rest);
decodeexecutionreport(Msg, [["167", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{securitytype=Value}, Rest);
decodeexecutionreport(Msg, [["22", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{idsource=Value}, Rest);
decodeexecutionreport(Msg, [["48", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{securityid=Value}, Rest);
decodeexecutionreport(Msg, [["65", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{symbolsfx=Value}, Rest);
decodeexecutionreport(Msg, [["55", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{symbol=Value}, Rest);
decodeexecutionreport(Msg, [["64", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{futsettdate=Value}, Rest);
decodeexecutionreport(Msg, [["63", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{settlmnttyp=Value}, Rest);
decodeexecutionreport(Msg, [["1", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{account=Value}, Rest);
decodeexecutionreport(Msg, [["378", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{execrestatementreason=Value}, Rest);
decodeexecutionreport(Msg, [["103", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{ordrejreason=Value}, Rest);
decodeexecutionreport(Msg, [["39", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{ordstatus=Value}, Rest);
decodeexecutionreport(Msg, [["150", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{exectype=Value}, Rest);
decodeexecutionreport(Msg, [["19", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{execrefid=Value}, Rest);
decodeexecutionreport(Msg, [["20", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{exectranstype=Value}, Rest);
decodeexecutionreport(Msg, [["17", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{execid=Value}, Rest);
decodeexecutionreport(Msg, [["66", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{listid=Value}, Rest);
decodeexecutionreport(Msg, [["76", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{execbroker=Value}, Rest);
decodeexecutionreport(Msg, [["109", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{clientid=Value}, Rest);
decodeexecutionreport(Msg, [["41", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{origclordid=Value}, Rest);
decodeexecutionreport(Msg, [["11", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{clordid=Value}, Rest);
decodeexecutionreport(Msg, [["198", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{secondaryorderid=Value}, Rest);
decodeexecutionreport(Msg, [["37", Value] | Rest]) -> 
    decodeexecutionreport(Msg#executionreport{orderid=Value}, Rest);
decodeexecutionreport(Msg, [[_, Value] | Rest]) ->
    decodeexecutionreport(Msg,Rest).

decodeordercancelreject(Msg,[]) -> 
    Msg;
decodeordercancelreject(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{onbehalfofsendingtime=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{lastmsgseqnumprocessed=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{messageencoding=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{xmldata=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{xmldatalen=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{origsendingtime=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{sendingtime=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{possresend=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{possdupflag=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{delivertolocationid=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{delivertosubid=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{onbehalfoflocationid=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{onbehalfofsubid=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{targetlocationid=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{targetsubid=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{senderlocationid=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{sendersubid=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{msgseqnum=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{securedata=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{securedatalen=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{delivertocompid=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{onbehalfofcompid=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{targetcompid=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{sendercompid=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{msgtype=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{bodylength=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#ordercancelreject.header#header{beginstring=Value},
    decodeordercancelreject(Msg#ordercancelreject{header=Header},Rest);
decodeordercancelreject(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#ordercancelreject.trailer#trailer{checksum=Value},
    decodeordercancelreject(Msg#ordercancelreject{trailer=Trailer},Rest);
decodeordercancelreject(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#ordercancelreject.trailer#trailer{signature=Value},
    decodeordercancelreject(Msg#ordercancelreject{trailer=Trailer},Rest);
decodeordercancelreject(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#ordercancelreject.trailer#trailer{signaturelength=Value},
    decodeordercancelreject(Msg#ordercancelreject{trailer=Trailer},Rest);
decodeordercancelreject(Msg, [["355", Value] | Rest]) -> 
    decodeordercancelreject(Msg#ordercancelreject{encodedtext=Value}, Rest);
decodeordercancelreject(Msg, [["354", Value] | Rest]) -> 
    decodeordercancelreject(Msg#ordercancelreject{encodedtextlen=Value}, Rest);
decodeordercancelreject(Msg, [["58", Value] | Rest]) -> 
    decodeordercancelreject(Msg#ordercancelreject{text=Value}, Rest);
decodeordercancelreject(Msg, [["102", Value] | Rest]) -> 
    decodeordercancelreject(Msg#ordercancelreject{cxlrejreason=Value}, Rest);
decodeordercancelreject(Msg, [["434", Value] | Rest]) -> 
    decodeordercancelreject(Msg#ordercancelreject{cxlrejresponseto=Value}, Rest);
decodeordercancelreject(Msg, [["60", Value] | Rest]) -> 
    decodeordercancelreject(Msg#ordercancelreject{transacttime=Value}, Rest);
decodeordercancelreject(Msg, [["1", Value] | Rest]) -> 
    decodeordercancelreject(Msg#ordercancelreject{account=Value}, Rest);
decodeordercancelreject(Msg, [["66", Value] | Rest]) -> 
    decodeordercancelreject(Msg#ordercancelreject{listid=Value}, Rest);
decodeordercancelreject(Msg, [["76", Value] | Rest]) -> 
    decodeordercancelreject(Msg#ordercancelreject{execbroker=Value}, Rest);
decodeordercancelreject(Msg, [["109", Value] | Rest]) -> 
    decodeordercancelreject(Msg#ordercancelreject{clientid=Value}, Rest);
decodeordercancelreject(Msg, [["39", Value] | Rest]) -> 
    decodeordercancelreject(Msg#ordercancelreject{ordstatus=Value}, Rest);
decodeordercancelreject(Msg, [["41", Value] | Rest]) -> 
    decodeordercancelreject(Msg#ordercancelreject{origclordid=Value}, Rest);
decodeordercancelreject(Msg, [["11", Value] | Rest]) -> 
    decodeordercancelreject(Msg#ordercancelreject{clordid=Value}, Rest);
decodeordercancelreject(Msg, [["198", Value] | Rest]) -> 
    decodeordercancelreject(Msg#ordercancelreject{secondaryorderid=Value}, Rest);
decodeordercancelreject(Msg, [["37", Value] | Rest]) -> 
    decodeordercancelreject(Msg#ordercancelreject{orderid=Value}, Rest);
decodeordercancelreject(Msg, [[_, Value] | Rest]) ->
    decodeordercancelreject(Msg,Rest).

decodelogon(Msg,[]) -> 
    Msg;
decodelogon(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#logon.header#header{onbehalfofsendingtime=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#logon.header#header{lastmsgseqnumprocessed=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#logon.header#header{messageencoding=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#logon.header#header{xmldata=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#logon.header#header{xmldatalen=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#logon.header#header{origsendingtime=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#logon.header#header{sendingtime=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#logon.header#header{possresend=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#logon.header#header{possdupflag=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#logon.header#header{delivertolocationid=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#logon.header#header{delivertosubid=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#logon.header#header{onbehalfoflocationid=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#logon.header#header{onbehalfofsubid=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#logon.header#header{targetlocationid=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#logon.header#header{targetsubid=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#logon.header#header{senderlocationid=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#logon.header#header{sendersubid=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#logon.header#header{msgseqnum=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#logon.header#header{securedata=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#logon.header#header{securedatalen=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#logon.header#header{delivertocompid=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#logon.header#header{onbehalfofcompid=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#logon.header#header{targetcompid=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#logon.header#header{sendercompid=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#logon.header#header{msgtype=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#logon.header#header{bodylength=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#logon.header#header{beginstring=Value},
    decodelogon(Msg#logon{header=Header},Rest);
decodelogon(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#logon.trailer#trailer{checksum=Value},
    decodelogon(Msg#logon{trailer=Trailer},Rest);
decodelogon(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#logon.trailer#trailer{signature=Value},
    decodelogon(Msg#logon{trailer=Trailer},Rest);
decodelogon(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#logon.trailer#trailer{signaturelength=Value},
    decodelogon(Msg#logon{trailer=Trailer},Rest);
decodelogon(Msg, [["383", Value] | Rest]) -> 
    decodelogon(Msg#logon{maxmessagesize=Value}, Rest);
decodelogon(Msg, [["141", Value] | Rest]) -> 
    decodelogon(Msg#logon{resetseqnumflag=Value}, Rest);
decodelogon(Msg, [["96", Value] | Rest]) -> 
    decodelogon(Msg#logon{rawdata=Value}, Rest);
decodelogon(Msg, [["95", Value] | Rest]) -> 
    decodelogon(Msg#logon{rawdatalength=Value}, Rest);
decodelogon(Msg, [["108", Value] | Rest]) -> 
    decodelogon(Msg#logon{heartbtint=Value}, Rest);
decodelogon(Msg, [["98", Value] | Rest]) -> 
    decodelogon(Msg#logon{encryptmethod=Value}, Rest);
decodelogon(Msg, [[_, Value] | Rest]) ->
    decodelogon(Msg,Rest).

decodenews(Msg,[]) -> 
    Msg;
decodenews(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#news.header#header{onbehalfofsendingtime=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#news.header#header{lastmsgseqnumprocessed=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#news.header#header{messageencoding=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#news.header#header{xmldata=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#news.header#header{xmldatalen=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#news.header#header{origsendingtime=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#news.header#header{sendingtime=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#news.header#header{possresend=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#news.header#header{possdupflag=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#news.header#header{delivertolocationid=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#news.header#header{delivertosubid=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#news.header#header{onbehalfoflocationid=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#news.header#header{onbehalfofsubid=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#news.header#header{targetlocationid=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#news.header#header{targetsubid=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#news.header#header{senderlocationid=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#news.header#header{sendersubid=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#news.header#header{msgseqnum=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#news.header#header{securedata=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#news.header#header{securedatalen=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#news.header#header{delivertocompid=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#news.header#header{onbehalfofcompid=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#news.header#header{targetcompid=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#news.header#header{sendercompid=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#news.header#header{msgtype=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#news.header#header{bodylength=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#news.header#header{beginstring=Value},
    decodenews(Msg#news{header=Header},Rest);
decodenews(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#news.trailer#trailer{checksum=Value},
    decodenews(Msg#news{trailer=Trailer},Rest);
decodenews(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#news.trailer#trailer{signature=Value},
    decodenews(Msg#news{trailer=Trailer},Rest);
decodenews(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#news.trailer#trailer{signaturelength=Value},
    decodenews(Msg#news{trailer=Trailer},Rest);
decodenews(Msg, [["96", Value] | Rest]) -> 
    decodenews(Msg#news{rawdata=Value}, Rest);
decodenews(Msg, [["95", Value] | Rest]) -> 
    decodenews(Msg#news{rawdatalength=Value}, Rest);
decodenews(Msg, [["149", Value] | Rest]) -> 
    decodenews(Msg#news{urllink=Value}, Rest);
decodenews(Msg, [["359", Value] | Rest]) -> 
    decodenews(Msg#news{encodedheadline=Value}, Rest);
decodenews(Msg, [["358", Value] | Rest]) -> 
    decodenews(Msg#news{encodedheadlinelen=Value}, Rest);
decodenews(Msg, [["148", Value] | Rest]) -> 
    decodenews(Msg#news{headline=Value}, Rest);
decodenews(Msg, [["61", Value] | Rest]) -> 
    decodenews(Msg#news{urgency=Value}, Rest);
decodenews(Msg, [["42", Value] | Rest]) -> 
    decodenews(Msg#news{origtime=Value}, Rest);
decodenews(Msg, [[_, Value] | Rest]) ->
    decodenews(Msg,Rest).

decodeemail(Msg,[]) -> 
    Msg;
decodeemail(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#email.header#header{onbehalfofsendingtime=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#email.header#header{lastmsgseqnumprocessed=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#email.header#header{messageencoding=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#email.header#header{xmldata=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#email.header#header{xmldatalen=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#email.header#header{origsendingtime=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#email.header#header{sendingtime=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#email.header#header{possresend=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#email.header#header{possdupflag=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#email.header#header{delivertolocationid=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#email.header#header{delivertosubid=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#email.header#header{onbehalfoflocationid=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#email.header#header{onbehalfofsubid=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#email.header#header{targetlocationid=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#email.header#header{targetsubid=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#email.header#header{senderlocationid=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#email.header#header{sendersubid=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#email.header#header{msgseqnum=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#email.header#header{securedata=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#email.header#header{securedatalen=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#email.header#header{delivertocompid=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#email.header#header{onbehalfofcompid=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#email.header#header{targetcompid=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#email.header#header{sendercompid=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#email.header#header{msgtype=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#email.header#header{bodylength=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#email.header#header{beginstring=Value},
    decodeemail(Msg#email{header=Header},Rest);
decodeemail(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#email.trailer#trailer{checksum=Value},
    decodeemail(Msg#email{trailer=Trailer},Rest);
decodeemail(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#email.trailer#trailer{signature=Value},
    decodeemail(Msg#email{trailer=Trailer},Rest);
decodeemail(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#email.trailer#trailer{signaturelength=Value},
    decodeemail(Msg#email{trailer=Trailer},Rest);
decodeemail(Msg, [["96", Value] | Rest]) -> 
    decodeemail(Msg#email{rawdata=Value}, Rest);
decodeemail(Msg, [["95", Value] | Rest]) -> 
    decodeemail(Msg#email{rawdatalength=Value}, Rest);
decodeemail(Msg, [["11", Value] | Rest]) -> 
    decodeemail(Msg#email{clordid=Value}, Rest);
decodeemail(Msg, [["37", Value] | Rest]) -> 
    decodeemail(Msg#email{orderid=Value}, Rest);
decodeemail(Msg, [["357", Value] | Rest]) -> 
    decodeemail(Msg#email{encodedsubject=Value}, Rest);
decodeemail(Msg, [["356", Value] | Rest]) -> 
    decodeemail(Msg#email{encodedsubjectlen=Value}, Rest);
decodeemail(Msg, [["147", Value] | Rest]) -> 
    decodeemail(Msg#email{subject=Value}, Rest);
decodeemail(Msg, [["42", Value] | Rest]) -> 
    decodeemail(Msg#email{origtime=Value}, Rest);
decodeemail(Msg, [["94", Value] | Rest]) -> 
    decodeemail(Msg#email{emailtype=Value}, Rest);
decodeemail(Msg, [["164", Value] | Rest]) -> 
    decodeemail(Msg#email{emailthreadid=Value}, Rest);
decodeemail(Msg, [[_, Value] | Rest]) ->
    decodeemail(Msg,Rest).

decodenewordersingle(Msg,[]) -> 
    Msg;
decodenewordersingle(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{onbehalfofsendingtime=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{lastmsgseqnumprocessed=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{messageencoding=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{xmldata=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{xmldatalen=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{origsendingtime=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{sendingtime=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{possresend=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{possdupflag=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{delivertolocationid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{delivertosubid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{onbehalfoflocationid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{onbehalfofsubid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{targetlocationid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{targetsubid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{senderlocationid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{sendersubid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{msgseqnum=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{securedata=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{securedatalen=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{delivertocompid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{onbehalfofcompid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{targetcompid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{sendercompid=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{msgtype=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{bodylength=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#newordersingle.header#header{beginstring=Value},
    decodenewordersingle(Msg#newordersingle{header=Header},Rest);
decodenewordersingle(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#newordersingle.trailer#trailer{checksum=Value},
    decodenewordersingle(Msg#newordersingle{trailer=Trailer},Rest);
decodenewordersingle(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#newordersingle.trailer#trailer{signature=Value},
    decodenewordersingle(Msg#newordersingle{trailer=Trailer},Rest);
decodenewordersingle(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#newordersingle.trailer#trailer{signaturelength=Value},
    decodenewordersingle(Msg#newordersingle{trailer=Trailer},Rest);
decodenewordersingle(Msg, [["440", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{clearingaccount=Value}, Rest);
decodenewordersingle(Msg, [["439", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{clearingfirm=Value}, Rest);
decodenewordersingle(Msg, [["389", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{discretionoffset=Value}, Rest);
decodenewordersingle(Msg, [["388", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{discretioninst=Value}, Rest);
decodenewordersingle(Msg, [["211", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{pegdifference=Value}, Rest);
decodenewordersingle(Msg, [["210", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{maxshow=Value}, Rest);
decodenewordersingle(Msg, [["204", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{customerorfirm=Value}, Rest);
decodenewordersingle(Msg, [["203", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{coveredoruncovered=Value}, Rest);
decodenewordersingle(Msg, [["77", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{openclose=Value}, Rest);
decodenewordersingle(Msg, [["192", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{orderqty2=Value}, Rest);
decodenewordersingle(Msg, [["193", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{futsettdate2=Value}, Rest);
decodenewordersingle(Msg, [["355", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{encodedtext=Value}, Rest);
decodenewordersingle(Msg, [["354", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{encodedtextlen=Value}, Rest);
decodenewordersingle(Msg, [["58", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{text=Value}, Rest);
decodenewordersingle(Msg, [["120", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{settlcurrency=Value}, Rest);
decodenewordersingle(Msg, [["121", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{forexreq=Value}, Rest);
decodenewordersingle(Msg, [["47", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{rule80a=Value}, Rest);
decodenewordersingle(Msg, [["13", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{commtype=Value}, Rest);
decodenewordersingle(Msg, [["12", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{commission=Value}, Rest);
decodenewordersingle(Msg, [["427", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{gtbookinginst=Value}, Rest);
decodenewordersingle(Msg, [["126", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{expiretime=Value}, Rest);
decodenewordersingle(Msg, [["432", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{expiredate=Value}, Rest);
decodenewordersingle(Msg, [["168", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{effectivetime=Value}, Rest);
decodenewordersingle(Msg, [["59", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{timeinforce=Value}, Rest);
decodenewordersingle(Msg, [["117", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{quoteid=Value}, Rest);
decodenewordersingle(Msg, [["23", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{ioiid=Value}, Rest);
decodenewordersingle(Msg, [["377", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{solicitedflag=Value}, Rest);
decodenewordersingle(Msg, [["376", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{complianceid=Value}, Rest);
decodenewordersingle(Msg, [["15", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{currency=Value}, Rest);
decodenewordersingle(Msg, [["99", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{stoppx=Value}, Rest);
decodenewordersingle(Msg, [["44", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{price=Value}, Rest);
decodenewordersingle(Msg, [["40", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{ordtype=Value}, Rest);
decodenewordersingle(Msg, [["152", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{cashorderqty=Value}, Rest);
decodenewordersingle(Msg, [["38", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{orderqty=Value}, Rest);
decodenewordersingle(Msg, [["60", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{transacttime=Value}, Rest);
decodenewordersingle(Msg, [["114", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{locatereqd=Value}, Rest);
decodenewordersingle(Msg, [["54", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{side=Value}, Rest);
decodenewordersingle(Msg, [["140", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{prevclosepx=Value}, Rest);
decodenewordersingle(Msg, [["351", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{encodedsecuritydesc=Value}, Rest);
decodenewordersingle(Msg, [["350", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{encodedsecuritydesclen=Value}, Rest);
decodenewordersingle(Msg, [["107", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{securitydesc=Value}, Rest);
decodenewordersingle(Msg, [["349", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{encodedissuer=Value}, Rest);
decodenewordersingle(Msg, [["348", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{encodedissuerlen=Value}, Rest);
decodenewordersingle(Msg, [["106", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{issuer=Value}, Rest);
decodenewordersingle(Msg, [["207", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{securityexchange=Value}, Rest);
decodenewordersingle(Msg, [["223", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{couponrate=Value}, Rest);
decodenewordersingle(Msg, [["231", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{contractmultiplier=Value}, Rest);
decodenewordersingle(Msg, [["206", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{optattribute=Value}, Rest);
decodenewordersingle(Msg, [["202", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{strikeprice=Value}, Rest);
decodenewordersingle(Msg, [["201", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{putorcall=Value}, Rest);
decodenewordersingle(Msg, [["205", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{maturityday=Value}, Rest);
decodenewordersingle(Msg, [["200", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{maturitymonthyear=Value}, Rest);
decodenewordersingle(Msg, [["167", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{securitytype=Value}, Rest);
decodenewordersingle(Msg, [["22", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{idsource=Value}, Rest);
decodenewordersingle(Msg, [["48", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{securityid=Value}, Rest);
decodenewordersingle(Msg, [["65", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{symbolsfx=Value}, Rest);
decodenewordersingle(Msg, [["55", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{symbol=Value}, Rest);
decodenewordersingle(Msg, [["81", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{processcode=Value}, Rest);
decodenewordersingle(Msg, [["100", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{exdestination=Value}, Rest);
decodenewordersingle(Msg, [["111", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{maxfloor=Value}, Rest);
decodenewordersingle(Msg, [["110", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{minqty=Value}, Rest);
decodenewordersingle(Msg, [["18", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{execinst=Value}, Rest);
decodenewordersingle(Msg, [["21", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{handlinst=Value}, Rest);
decodenewordersingle(Msg, [["64", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{futsettdate=Value}, Rest);
decodenewordersingle(Msg, [["63", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{settlmnttyp=Value}, Rest);
decodenewordersingle(Msg, [["1", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{account=Value}, Rest);
decodenewordersingle(Msg, [["76", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{execbroker=Value}, Rest);
decodenewordersingle(Msg, [["109", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{clientid=Value}, Rest);
decodenewordersingle(Msg, [["11", Value] | Rest]) -> 
    decodenewordersingle(Msg#newordersingle{clordid=Value}, Rest);
decodenewordersingle(Msg, [[_, Value] | Rest]) ->
    decodenewordersingle(Msg,Rest).

decodeneworderlist(Msg,[]) -> 
    Msg;
decodeneworderlist(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{onbehalfofsendingtime=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{lastmsgseqnumprocessed=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{messageencoding=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{xmldata=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{xmldatalen=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{origsendingtime=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{sendingtime=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{possresend=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{possdupflag=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{delivertolocationid=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{delivertosubid=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{onbehalfoflocationid=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{onbehalfofsubid=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{targetlocationid=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{targetsubid=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{senderlocationid=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{sendersubid=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{msgseqnum=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{securedata=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{securedatalen=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{delivertocompid=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{onbehalfofcompid=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{targetcompid=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{sendercompid=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{msgtype=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{bodylength=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#neworderlist.header#header{beginstring=Value},
    decodeneworderlist(Msg#neworderlist{header=Header},Rest);
decodeneworderlist(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#neworderlist.trailer#trailer{checksum=Value},
    decodeneworderlist(Msg#neworderlist{trailer=Trailer},Rest);
decodeneworderlist(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#neworderlist.trailer#trailer{signature=Value},
    decodeneworderlist(Msg#neworderlist{trailer=Trailer},Rest);
decodeneworderlist(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#neworderlist.trailer#trailer{signaturelength=Value},
    decodeneworderlist(Msg#neworderlist{trailer=Trailer},Rest);
decodeneworderlist(Msg, [["68", Value] | Rest]) -> 
    decodeneworderlist(Msg#neworderlist{totnoorders=Value}, Rest);
decodeneworderlist(Msg, [["353", Value] | Rest]) -> 
    decodeneworderlist(Msg#neworderlist{encodedlistexecinst=Value}, Rest);
decodeneworderlist(Msg, [["352", Value] | Rest]) -> 
    decodeneworderlist(Msg#neworderlist{encodedlistexecinstlen=Value}, Rest);
decodeneworderlist(Msg, [["69", Value] | Rest]) -> 
    decodeneworderlist(Msg#neworderlist{listexecinst=Value}, Rest);
decodeneworderlist(Msg, [["433", Value] | Rest]) -> 
    decodeneworderlist(Msg#neworderlist{listexecinsttype=Value}, Rest);
decodeneworderlist(Msg, [["415", Value] | Rest]) -> 
    decodeneworderlist(Msg#neworderlist{progperiodinterval=Value}, Rest);
decodeneworderlist(Msg, [["394", Value] | Rest]) -> 
    decodeneworderlist(Msg#neworderlist{bidtype=Value}, Rest);
decodeneworderlist(Msg, [["414", Value] | Rest]) -> 
    decodeneworderlist(Msg#neworderlist{progrptreqs=Value}, Rest);
decodeneworderlist(Msg, [["391", Value] | Rest]) -> 
    decodeneworderlist(Msg#neworderlist{clientbidid=Value}, Rest);
decodeneworderlist(Msg, [["390", Value] | Rest]) -> 
    decodeneworderlist(Msg#neworderlist{bidid=Value}, Rest);
decodeneworderlist(Msg, [["66", Value] | Rest]) -> 
    decodeneworderlist(Msg#neworderlist{listid=Value}, Rest);
decodeneworderlist(Msg, [[_, Value] | Rest]) ->
    decodeneworderlist(Msg,Rest).

decodeordercancelrequest(Msg,[]) -> 
    Msg;
decodeordercancelrequest(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{onbehalfofsendingtime=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{lastmsgseqnumprocessed=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{messageencoding=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{xmldata=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{xmldatalen=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{origsendingtime=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{sendingtime=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{possresend=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{possdupflag=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{delivertolocationid=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{delivertosubid=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{onbehalfoflocationid=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{onbehalfofsubid=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{targetlocationid=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{targetsubid=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{senderlocationid=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{sendersubid=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{msgseqnum=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{securedata=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{securedatalen=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{delivertocompid=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{onbehalfofcompid=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{targetcompid=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{sendercompid=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{msgtype=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{bodylength=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#ordercancelrequest.header#header{beginstring=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{header=Header},Rest);
decodeordercancelrequest(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#ordercancelrequest.trailer#trailer{checksum=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{trailer=Trailer},Rest);
decodeordercancelrequest(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#ordercancelrequest.trailer#trailer{signature=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{trailer=Trailer},Rest);
decodeordercancelrequest(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#ordercancelrequest.trailer#trailer{signaturelength=Value},
    decodeordercancelrequest(Msg#ordercancelrequest{trailer=Trailer},Rest);
decodeordercancelrequest(Msg, [["355", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{encodedtext=Value}, Rest);
decodeordercancelrequest(Msg, [["354", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{encodedtextlen=Value}, Rest);
decodeordercancelrequest(Msg, [["58", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{text=Value}, Rest);
decodeordercancelrequest(Msg, [["377", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{solicitedflag=Value}, Rest);
decodeordercancelrequest(Msg, [["376", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{complianceid=Value}, Rest);
decodeordercancelrequest(Msg, [["152", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{cashorderqty=Value}, Rest);
decodeordercancelrequest(Msg, [["38", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{orderqty=Value}, Rest);
decodeordercancelrequest(Msg, [["60", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{transacttime=Value}, Rest);
decodeordercancelrequest(Msg, [["54", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{side=Value}, Rest);
decodeordercancelrequest(Msg, [["351", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{encodedsecuritydesc=Value}, Rest);
decodeordercancelrequest(Msg, [["350", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{encodedsecuritydesclen=Value}, Rest);
decodeordercancelrequest(Msg, [["107", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{securitydesc=Value}, Rest);
decodeordercancelrequest(Msg, [["349", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{encodedissuer=Value}, Rest);
decodeordercancelrequest(Msg, [["348", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{encodedissuerlen=Value}, Rest);
decodeordercancelrequest(Msg, [["106", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{issuer=Value}, Rest);
decodeordercancelrequest(Msg, [["207", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{securityexchange=Value}, Rest);
decodeordercancelrequest(Msg, [["223", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{couponrate=Value}, Rest);
decodeordercancelrequest(Msg, [["231", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{contractmultiplier=Value}, Rest);
decodeordercancelrequest(Msg, [["206", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{optattribute=Value}, Rest);
decodeordercancelrequest(Msg, [["202", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{strikeprice=Value}, Rest);
decodeordercancelrequest(Msg, [["201", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{putorcall=Value}, Rest);
decodeordercancelrequest(Msg, [["205", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{maturityday=Value}, Rest);
decodeordercancelrequest(Msg, [["200", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{maturitymonthyear=Value}, Rest);
decodeordercancelrequest(Msg, [["167", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{securitytype=Value}, Rest);
decodeordercancelrequest(Msg, [["22", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{idsource=Value}, Rest);
decodeordercancelrequest(Msg, [["48", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{securityid=Value}, Rest);
decodeordercancelrequest(Msg, [["65", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{symbolsfx=Value}, Rest);
decodeordercancelrequest(Msg, [["55", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{symbol=Value}, Rest);
decodeordercancelrequest(Msg, [["76", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{execbroker=Value}, Rest);
decodeordercancelrequest(Msg, [["109", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{clientid=Value}, Rest);
decodeordercancelrequest(Msg, [["1", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{account=Value}, Rest);
decodeordercancelrequest(Msg, [["66", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{listid=Value}, Rest);
decodeordercancelrequest(Msg, [["11", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{clordid=Value}, Rest);
decodeordercancelrequest(Msg, [["37", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{orderid=Value}, Rest);
decodeordercancelrequest(Msg, [["41", Value] | Rest]) -> 
    decodeordercancelrequest(Msg#ordercancelrequest{origclordid=Value}, Rest);
decodeordercancelrequest(Msg, [[_, Value] | Rest]) ->
    decodeordercancelrequest(Msg,Rest).

decodeordercancelreplacerequest(Msg,[]) -> 
    Msg;
decodeordercancelreplacerequest(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{onbehalfofsendingtime=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{lastmsgseqnumprocessed=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{messageencoding=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{xmldata=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{xmldatalen=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{origsendingtime=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{sendingtime=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{possresend=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{possdupflag=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{delivertolocationid=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{delivertosubid=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{onbehalfoflocationid=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{onbehalfofsubid=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{targetlocationid=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{targetsubid=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{senderlocationid=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{sendersubid=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{msgseqnum=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{securedata=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{securedatalen=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{delivertocompid=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{onbehalfofcompid=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{targetcompid=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{sendercompid=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{msgtype=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{bodylength=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#ordercancelreplacerequest.header#header{beginstring=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{header=Header},Rest);
decodeordercancelreplacerequest(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#ordercancelreplacerequest.trailer#trailer{checksum=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{trailer=Trailer},Rest);
decodeordercancelreplacerequest(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#ordercancelreplacerequest.trailer#trailer{signature=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{trailer=Trailer},Rest);
decodeordercancelreplacerequest(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#ordercancelreplacerequest.trailer#trailer{signaturelength=Value},
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{trailer=Trailer},Rest);
decodeordercancelreplacerequest(Msg, [["440", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{clearingaccount=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["439", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{clearingfirm=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["114", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{locatereqd=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["210", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{maxshow=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["204", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{customerorfirm=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["203", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{coveredoruncovered=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["77", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{openclose=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["192", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{orderqty2=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["193", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{futsettdate2=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["355", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{encodedtext=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["354", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{encodedtextlen=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["58", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{text=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["120", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{settlcurrency=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["121", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{forexreq=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["47", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{rule80a=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["13", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{commtype=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["12", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{commission=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["427", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{gtbookinginst=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["126", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{expiretime=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["432", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{expiredate=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["168", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{effectivetime=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["59", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{timeinforce=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["15", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{currency=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["377", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{solicitedflag=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["376", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{complianceid=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["389", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{discretionoffset=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["388", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{discretioninst=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["211", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{pegdifference=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["99", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{stoppx=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["44", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{price=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["40", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{ordtype=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["152", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{cashorderqty=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["38", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{orderqty=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["60", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{transacttime=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["54", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{side=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["351", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{encodedsecuritydesc=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["350", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{encodedsecuritydesclen=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["107", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{securitydesc=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["349", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{encodedissuer=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["348", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{encodedissuerlen=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["106", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{issuer=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["207", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{securityexchange=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["223", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{couponrate=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["231", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{contractmultiplier=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["206", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{optattribute=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["202", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{strikeprice=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["201", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{putorcall=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["205", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{maturityday=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["200", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{maturitymonthyear=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["167", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{securitytype=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["22", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{idsource=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["48", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{securityid=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["65", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{symbolsfx=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["55", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{symbol=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["100", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{exdestination=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["111", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{maxfloor=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["110", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{minqty=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["18", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{execinst=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["21", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{handlinst=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["64", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{futsettdate=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["63", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{settlmnttyp=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["1", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{account=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["66", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{listid=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["11", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{clordid=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["41", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{origclordid=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["76", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{execbroker=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["109", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{clientid=Value}, Rest);
decodeordercancelreplacerequest(Msg, [["37", Value] | Rest]) -> 
    decodeordercancelreplacerequest(Msg#ordercancelreplacerequest{orderid=Value}, Rest);
decodeordercancelreplacerequest(Msg, [[_, Value] | Rest]) ->
    decodeordercancelreplacerequest(Msg,Rest).

decodeorderstatusrequest(Msg,[]) -> 
    Msg;
decodeorderstatusrequest(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{onbehalfofsendingtime=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{lastmsgseqnumprocessed=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{messageencoding=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{xmldata=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{xmldatalen=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{origsendingtime=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{sendingtime=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{possresend=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{possdupflag=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{delivertolocationid=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{delivertosubid=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{onbehalfoflocationid=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{onbehalfofsubid=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{targetlocationid=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{targetsubid=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{senderlocationid=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{sendersubid=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{msgseqnum=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{securedata=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{securedatalen=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{delivertocompid=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{onbehalfofcompid=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{targetcompid=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{sendercompid=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{msgtype=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{bodylength=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#orderstatusrequest.header#header{beginstring=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{header=Header},Rest);
decodeorderstatusrequest(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#orderstatusrequest.trailer#trailer{checksum=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{trailer=Trailer},Rest);
decodeorderstatusrequest(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#orderstatusrequest.trailer#trailer{signature=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{trailer=Trailer},Rest);
decodeorderstatusrequest(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#orderstatusrequest.trailer#trailer{signaturelength=Value},
    decodeorderstatusrequest(Msg#orderstatusrequest{trailer=Trailer},Rest);
decodeorderstatusrequest(Msg, [["54", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{side=Value}, Rest);
decodeorderstatusrequest(Msg, [["351", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{encodedsecuritydesc=Value}, Rest);
decodeorderstatusrequest(Msg, [["350", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{encodedsecuritydesclen=Value}, Rest);
decodeorderstatusrequest(Msg, [["107", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{securitydesc=Value}, Rest);
decodeorderstatusrequest(Msg, [["349", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{encodedissuer=Value}, Rest);
decodeorderstatusrequest(Msg, [["348", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{encodedissuerlen=Value}, Rest);
decodeorderstatusrequest(Msg, [["106", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{issuer=Value}, Rest);
decodeorderstatusrequest(Msg, [["207", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{securityexchange=Value}, Rest);
decodeorderstatusrequest(Msg, [["223", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{couponrate=Value}, Rest);
decodeorderstatusrequest(Msg, [["231", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{contractmultiplier=Value}, Rest);
decodeorderstatusrequest(Msg, [["206", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{optattribute=Value}, Rest);
decodeorderstatusrequest(Msg, [["202", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{strikeprice=Value}, Rest);
decodeorderstatusrequest(Msg, [["201", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{putorcall=Value}, Rest);
decodeorderstatusrequest(Msg, [["205", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{maturityday=Value}, Rest);
decodeorderstatusrequest(Msg, [["200", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{maturitymonthyear=Value}, Rest);
decodeorderstatusrequest(Msg, [["167", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{securitytype=Value}, Rest);
decodeorderstatusrequest(Msg, [["22", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{idsource=Value}, Rest);
decodeorderstatusrequest(Msg, [["48", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{securityid=Value}, Rest);
decodeorderstatusrequest(Msg, [["65", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{symbolsfx=Value}, Rest);
decodeorderstatusrequest(Msg, [["55", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{symbol=Value}, Rest);
decodeorderstatusrequest(Msg, [["76", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{execbroker=Value}, Rest);
decodeorderstatusrequest(Msg, [["1", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{account=Value}, Rest);
decodeorderstatusrequest(Msg, [["109", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{clientid=Value}, Rest);
decodeorderstatusrequest(Msg, [["11", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{clordid=Value}, Rest);
decodeorderstatusrequest(Msg, [["37", Value] | Rest]) -> 
    decodeorderstatusrequest(Msg#orderstatusrequest{orderid=Value}, Rest);
decodeorderstatusrequest(Msg, [[_, Value] | Rest]) ->
    decodeorderstatusrequest(Msg,Rest).

decodeallocation(Msg,[]) -> 
    Msg;
decodeallocation(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{onbehalfofsendingtime=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{lastmsgseqnumprocessed=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{messageencoding=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{xmldata=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{xmldatalen=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{origsendingtime=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{sendingtime=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{possresend=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{possdupflag=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{delivertolocationid=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{delivertosubid=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{onbehalfoflocationid=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{onbehalfofsubid=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{targetlocationid=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{targetsubid=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{senderlocationid=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{sendersubid=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{msgseqnum=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{securedata=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{securedatalen=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{delivertocompid=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{onbehalfofcompid=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{targetcompid=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{sendercompid=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{msgtype=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{bodylength=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#allocation.header#header{beginstring=Value},
    decodeallocation(Msg#allocation{header=Header},Rest);
decodeallocation(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#allocation.trailer#trailer{checksum=Value},
    decodeallocation(Msg#allocation{trailer=Trailer},Rest);
decodeallocation(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#allocation.trailer#trailer{signature=Value},
    decodeallocation(Msg#allocation{trailer=Trailer},Rest);
decodeallocation(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#allocation.trailer#trailer{signaturelength=Value},
    decodeallocation(Msg#allocation{trailer=Trailer},Rest);
decodeallocation(Msg, [["158", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{accruedinterestrate=Value}, Rest);
decodeallocation(Msg, [["157", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{numdaysinterest=Value}, Rest);
decodeallocation(Msg, [["355", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{encodedtext=Value}, Rest);
decodeallocation(Msg, [["354", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{encodedtextlen=Value}, Rest);
decodeallocation(Msg, [["58", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{text=Value}, Rest);
decodeallocation(Msg, [["77", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{openclose=Value}, Rest);
decodeallocation(Msg, [["118", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{netmoney=Value}, Rest);
decodeallocation(Msg, [["381", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{grosstradeamt=Value}, Rest);
decodeallocation(Msg, [["64", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{futsettdate=Value}, Rest);
decodeallocation(Msg, [["63", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{settlmnttyp=Value}, Rest);
decodeallocation(Msg, [["60", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{transacttime=Value}, Rest);
decodeallocation(Msg, [["75", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{tradedate=Value}, Rest);
decodeallocation(Msg, [["74", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{avgprxprecision=Value}, Rest);
decodeallocation(Msg, [["15", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{currency=Value}, Rest);
decodeallocation(Msg, [["6", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{avgpx=Value}, Rest);
decodeallocation(Msg, [["336", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{tradingsessionid=Value}, Rest);
decodeallocation(Msg, [["30", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{lastmkt=Value}, Rest);
decodeallocation(Msg, [["53", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{shares=Value}, Rest);
decodeallocation(Msg, [["351", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{encodedsecuritydesc=Value}, Rest);
decodeallocation(Msg, [["350", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{encodedsecuritydesclen=Value}, Rest);
decodeallocation(Msg, [["107", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{securitydesc=Value}, Rest);
decodeallocation(Msg, [["349", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{encodedissuer=Value}, Rest);
decodeallocation(Msg, [["348", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{encodedissuerlen=Value}, Rest);
decodeallocation(Msg, [["106", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{issuer=Value}, Rest);
decodeallocation(Msg, [["207", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{securityexchange=Value}, Rest);
decodeallocation(Msg, [["223", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{couponrate=Value}, Rest);
decodeallocation(Msg, [["231", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{contractmultiplier=Value}, Rest);
decodeallocation(Msg, [["206", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{optattribute=Value}, Rest);
decodeallocation(Msg, [["202", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{strikeprice=Value}, Rest);
decodeallocation(Msg, [["201", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{putorcall=Value}, Rest);
decodeallocation(Msg, [["205", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{maturityday=Value}, Rest);
decodeallocation(Msg, [["200", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{maturitymonthyear=Value}, Rest);
decodeallocation(Msg, [["167", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{securitytype=Value}, Rest);
decodeallocation(Msg, [["22", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{idsource=Value}, Rest);
decodeallocation(Msg, [["48", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{securityid=Value}, Rest);
decodeallocation(Msg, [["65", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{symbolsfx=Value}, Rest);
decodeallocation(Msg, [["55", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{symbol=Value}, Rest);
decodeallocation(Msg, [["54", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{side=Value}, Rest);
decodeallocation(Msg, [["197", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{alloclinktype=Value}, Rest);
decodeallocation(Msg, [["196", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{alloclinkid=Value}, Rest);
decodeallocation(Msg, [["72", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{refallocid=Value}, Rest);
decodeallocation(Msg, [["71", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{alloctranstype=Value}, Rest);
decodeallocation(Msg, [["70", Value] | Rest]) -> 
    decodeallocation(Msg#allocation{allocid=Value}, Rest);
decodeallocation(Msg, [[_, Value] | Rest]) ->
    decodeallocation(Msg,Rest).

decodelistcancelrequest(Msg,[]) -> 
    Msg;
decodelistcancelrequest(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{onbehalfofsendingtime=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{lastmsgseqnumprocessed=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{messageencoding=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{xmldata=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{xmldatalen=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{origsendingtime=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{sendingtime=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{possresend=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{possdupflag=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{delivertolocationid=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{delivertosubid=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{onbehalfoflocationid=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{onbehalfofsubid=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{targetlocationid=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{targetsubid=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{senderlocationid=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{sendersubid=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{msgseqnum=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{securedata=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{securedatalen=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{delivertocompid=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{onbehalfofcompid=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{targetcompid=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{sendercompid=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{msgtype=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{bodylength=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#listcancelrequest.header#header{beginstring=Value},
    decodelistcancelrequest(Msg#listcancelrequest{header=Header},Rest);
decodelistcancelrequest(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#listcancelrequest.trailer#trailer{checksum=Value},
    decodelistcancelrequest(Msg#listcancelrequest{trailer=Trailer},Rest);
decodelistcancelrequest(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#listcancelrequest.trailer#trailer{signature=Value},
    decodelistcancelrequest(Msg#listcancelrequest{trailer=Trailer},Rest);
decodelistcancelrequest(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#listcancelrequest.trailer#trailer{signaturelength=Value},
    decodelistcancelrequest(Msg#listcancelrequest{trailer=Trailer},Rest);
decodelistcancelrequest(Msg, [["355", Value] | Rest]) -> 
    decodelistcancelrequest(Msg#listcancelrequest{encodedtext=Value}, Rest);
decodelistcancelrequest(Msg, [["354", Value] | Rest]) -> 
    decodelistcancelrequest(Msg#listcancelrequest{encodedtextlen=Value}, Rest);
decodelistcancelrequest(Msg, [["58", Value] | Rest]) -> 
    decodelistcancelrequest(Msg#listcancelrequest{text=Value}, Rest);
decodelistcancelrequest(Msg, [["60", Value] | Rest]) -> 
    decodelistcancelrequest(Msg#listcancelrequest{transacttime=Value}, Rest);
decodelistcancelrequest(Msg, [["66", Value] | Rest]) -> 
    decodelistcancelrequest(Msg#listcancelrequest{listid=Value}, Rest);
decodelistcancelrequest(Msg, [[_, Value] | Rest]) ->
    decodelistcancelrequest(Msg,Rest).

decodelistexecute(Msg,[]) -> 
    Msg;
decodelistexecute(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{onbehalfofsendingtime=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{lastmsgseqnumprocessed=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{messageencoding=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{xmldata=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{xmldatalen=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{origsendingtime=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{sendingtime=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{possresend=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{possdupflag=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{delivertolocationid=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{delivertosubid=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{onbehalfoflocationid=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{onbehalfofsubid=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{targetlocationid=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{targetsubid=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{senderlocationid=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{sendersubid=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{msgseqnum=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{securedata=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{securedatalen=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{delivertocompid=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{onbehalfofcompid=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{targetcompid=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{sendercompid=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{msgtype=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{bodylength=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#listexecute.header#header{beginstring=Value},
    decodelistexecute(Msg#listexecute{header=Header},Rest);
decodelistexecute(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#listexecute.trailer#trailer{checksum=Value},
    decodelistexecute(Msg#listexecute{trailer=Trailer},Rest);
decodelistexecute(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#listexecute.trailer#trailer{signature=Value},
    decodelistexecute(Msg#listexecute{trailer=Trailer},Rest);
decodelistexecute(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#listexecute.trailer#trailer{signaturelength=Value},
    decodelistexecute(Msg#listexecute{trailer=Trailer},Rest);
decodelistexecute(Msg, [["355", Value] | Rest]) -> 
    decodelistexecute(Msg#listexecute{encodedtext=Value}, Rest);
decodelistexecute(Msg, [["354", Value] | Rest]) -> 
    decodelistexecute(Msg#listexecute{encodedtextlen=Value}, Rest);
decodelistexecute(Msg, [["58", Value] | Rest]) -> 
    decodelistexecute(Msg#listexecute{text=Value}, Rest);
decodelistexecute(Msg, [["60", Value] | Rest]) -> 
    decodelistexecute(Msg#listexecute{transacttime=Value}, Rest);
decodelistexecute(Msg, [["390", Value] | Rest]) -> 
    decodelistexecute(Msg#listexecute{bidid=Value}, Rest);
decodelistexecute(Msg, [["391", Value] | Rest]) -> 
    decodelistexecute(Msg#listexecute{clientbidid=Value}, Rest);
decodelistexecute(Msg, [["66", Value] | Rest]) -> 
    decodelistexecute(Msg#listexecute{listid=Value}, Rest);
decodelistexecute(Msg, [[_, Value] | Rest]) ->
    decodelistexecute(Msg,Rest).

decodeliststatusrequest(Msg,[]) -> 
    Msg;
decodeliststatusrequest(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{onbehalfofsendingtime=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{lastmsgseqnumprocessed=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{messageencoding=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{xmldata=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{xmldatalen=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{origsendingtime=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{sendingtime=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{possresend=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{possdupflag=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{delivertolocationid=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{delivertosubid=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{onbehalfoflocationid=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{onbehalfofsubid=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{targetlocationid=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{targetsubid=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{senderlocationid=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{sendersubid=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{msgseqnum=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{securedata=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{securedatalen=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{delivertocompid=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{onbehalfofcompid=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{targetcompid=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{sendercompid=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{msgtype=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{bodylength=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#liststatusrequest.header#header{beginstring=Value},
    decodeliststatusrequest(Msg#liststatusrequest{header=Header},Rest);
decodeliststatusrequest(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#liststatusrequest.trailer#trailer{checksum=Value},
    decodeliststatusrequest(Msg#liststatusrequest{trailer=Trailer},Rest);
decodeliststatusrequest(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#liststatusrequest.trailer#trailer{signature=Value},
    decodeliststatusrequest(Msg#liststatusrequest{trailer=Trailer},Rest);
decodeliststatusrequest(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#liststatusrequest.trailer#trailer{signaturelength=Value},
    decodeliststatusrequest(Msg#liststatusrequest{trailer=Trailer},Rest);
decodeliststatusrequest(Msg, [["355", Value] | Rest]) -> 
    decodeliststatusrequest(Msg#liststatusrequest{encodedtext=Value}, Rest);
decodeliststatusrequest(Msg, [["354", Value] | Rest]) -> 
    decodeliststatusrequest(Msg#liststatusrequest{encodedtextlen=Value}, Rest);
decodeliststatusrequest(Msg, [["58", Value] | Rest]) -> 
    decodeliststatusrequest(Msg#liststatusrequest{text=Value}, Rest);
decodeliststatusrequest(Msg, [["66", Value] | Rest]) -> 
    decodeliststatusrequest(Msg#liststatusrequest{listid=Value}, Rest);
decodeliststatusrequest(Msg, [[_, Value] | Rest]) ->
    decodeliststatusrequest(Msg,Rest).

decodeliststatus(Msg,[]) -> 
    Msg;
decodeliststatus(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{onbehalfofsendingtime=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{lastmsgseqnumprocessed=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{messageencoding=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{xmldata=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{xmldatalen=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{origsendingtime=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{sendingtime=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{possresend=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{possdupflag=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{delivertolocationid=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{delivertosubid=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{onbehalfoflocationid=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{onbehalfofsubid=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{targetlocationid=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{targetsubid=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{senderlocationid=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{sendersubid=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{msgseqnum=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{securedata=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{securedatalen=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{delivertocompid=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{onbehalfofcompid=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{targetcompid=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{sendercompid=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{msgtype=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{bodylength=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#liststatus.header#header{beginstring=Value},
    decodeliststatus(Msg#liststatus{header=Header},Rest);
decodeliststatus(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#liststatus.trailer#trailer{checksum=Value},
    decodeliststatus(Msg#liststatus{trailer=Trailer},Rest);
decodeliststatus(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#liststatus.trailer#trailer{signature=Value},
    decodeliststatus(Msg#liststatus{trailer=Trailer},Rest);
decodeliststatus(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#liststatus.trailer#trailer{signaturelength=Value},
    decodeliststatus(Msg#liststatus{trailer=Trailer},Rest);
decodeliststatus(Msg, [["68", Value] | Rest]) -> 
    decodeliststatus(Msg#liststatus{totnoorders=Value}, Rest);
decodeliststatus(Msg, [["60", Value] | Rest]) -> 
    decodeliststatus(Msg#liststatus{transacttime=Value}, Rest);
decodeliststatus(Msg, [["446", Value] | Rest]) -> 
    decodeliststatus(Msg#liststatus{encodedliststatustext=Value}, Rest);
decodeliststatus(Msg, [["445", Value] | Rest]) -> 
    decodeliststatus(Msg#liststatus{encodedliststatustextlen=Value}, Rest);
decodeliststatus(Msg, [["444", Value] | Rest]) -> 
    decodeliststatus(Msg#liststatus{liststatustext=Value}, Rest);
decodeliststatus(Msg, [["83", Value] | Rest]) -> 
    decodeliststatus(Msg#liststatus{rptseq=Value}, Rest);
decodeliststatus(Msg, [["431", Value] | Rest]) -> 
    decodeliststatus(Msg#liststatus{listorderstatus=Value}, Rest);
decodeliststatus(Msg, [["82", Value] | Rest]) -> 
    decodeliststatus(Msg#liststatus{norpts=Value}, Rest);
decodeliststatus(Msg, [["429", Value] | Rest]) -> 
    decodeliststatus(Msg#liststatus{liststatustype=Value}, Rest);
decodeliststatus(Msg, [["66", Value] | Rest]) -> 
    decodeliststatus(Msg#liststatus{listid=Value}, Rest);
decodeliststatus(Msg, [[_, Value] | Rest]) ->
    decodeliststatus(Msg,Rest).

decodeallocationack(Msg,[]) -> 
    Msg;
decodeallocationack(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{onbehalfofsendingtime=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{lastmsgseqnumprocessed=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{messageencoding=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{xmldata=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{xmldatalen=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{origsendingtime=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{sendingtime=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{possresend=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{possdupflag=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{delivertolocationid=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{delivertosubid=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{onbehalfoflocationid=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{onbehalfofsubid=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{targetlocationid=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{targetsubid=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{senderlocationid=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{sendersubid=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{msgseqnum=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{securedata=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{securedatalen=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{delivertocompid=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{onbehalfofcompid=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{targetcompid=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{sendercompid=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{msgtype=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{bodylength=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#allocationack.header#header{beginstring=Value},
    decodeallocationack(Msg#allocationack{header=Header},Rest);
decodeallocationack(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#allocationack.trailer#trailer{checksum=Value},
    decodeallocationack(Msg#allocationack{trailer=Trailer},Rest);
decodeallocationack(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#allocationack.trailer#trailer{signature=Value},
    decodeallocationack(Msg#allocationack{trailer=Trailer},Rest);
decodeallocationack(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#allocationack.trailer#trailer{signaturelength=Value},
    decodeallocationack(Msg#allocationack{trailer=Trailer},Rest);
decodeallocationack(Msg, [["355", Value] | Rest]) -> 
    decodeallocationack(Msg#allocationack{encodedtext=Value}, Rest);
decodeallocationack(Msg, [["354", Value] | Rest]) -> 
    decodeallocationack(Msg#allocationack{encodedtextlen=Value}, Rest);
decodeallocationack(Msg, [["58", Value] | Rest]) -> 
    decodeallocationack(Msg#allocationack{text=Value}, Rest);
decodeallocationack(Msg, [["88", Value] | Rest]) -> 
    decodeallocationack(Msg#allocationack{allocrejcode=Value}, Rest);
decodeallocationack(Msg, [["87", Value] | Rest]) -> 
    decodeallocationack(Msg#allocationack{allocstatus=Value}, Rest);
decodeallocationack(Msg, [["60", Value] | Rest]) -> 
    decodeallocationack(Msg#allocationack{transacttime=Value}, Rest);
decodeallocationack(Msg, [["75", Value] | Rest]) -> 
    decodeallocationack(Msg#allocationack{tradedate=Value}, Rest);
decodeallocationack(Msg, [["70", Value] | Rest]) -> 
    decodeallocationack(Msg#allocationack{allocid=Value}, Rest);
decodeallocationack(Msg, [["76", Value] | Rest]) -> 
    decodeallocationack(Msg#allocationack{execbroker=Value}, Rest);
decodeallocationack(Msg, [["109", Value] | Rest]) -> 
    decodeallocationack(Msg#allocationack{clientid=Value}, Rest);
decodeallocationack(Msg, [[_, Value] | Rest]) ->
    decodeallocationack(Msg,Rest).

decodedontknowtrade(Msg,[]) -> 
    Msg;
decodedontknowtrade(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{onbehalfofsendingtime=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{lastmsgseqnumprocessed=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{messageencoding=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{xmldata=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{xmldatalen=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{origsendingtime=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{sendingtime=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{possresend=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{possdupflag=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{delivertolocationid=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{delivertosubid=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{onbehalfoflocationid=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{onbehalfofsubid=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{targetlocationid=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{targetsubid=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{senderlocationid=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{sendersubid=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{msgseqnum=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{securedata=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{securedatalen=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{delivertocompid=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{onbehalfofcompid=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{targetcompid=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{sendercompid=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{msgtype=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{bodylength=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#dontknowtrade.header#header{beginstring=Value},
    decodedontknowtrade(Msg#dontknowtrade{header=Header},Rest);
decodedontknowtrade(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#dontknowtrade.trailer#trailer{checksum=Value},
    decodedontknowtrade(Msg#dontknowtrade{trailer=Trailer},Rest);
decodedontknowtrade(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#dontknowtrade.trailer#trailer{signature=Value},
    decodedontknowtrade(Msg#dontknowtrade{trailer=Trailer},Rest);
decodedontknowtrade(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#dontknowtrade.trailer#trailer{signaturelength=Value},
    decodedontknowtrade(Msg#dontknowtrade{trailer=Trailer},Rest);
decodedontknowtrade(Msg, [["355", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{encodedtext=Value}, Rest);
decodedontknowtrade(Msg, [["354", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{encodedtextlen=Value}, Rest);
decodedontknowtrade(Msg, [["58", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{text=Value}, Rest);
decodedontknowtrade(Msg, [["31", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{lastpx=Value}, Rest);
decodedontknowtrade(Msg, [["32", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{lastshares=Value}, Rest);
decodedontknowtrade(Msg, [["152", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{cashorderqty=Value}, Rest);
decodedontknowtrade(Msg, [["38", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{orderqty=Value}, Rest);
decodedontknowtrade(Msg, [["54", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{side=Value}, Rest);
decodedontknowtrade(Msg, [["351", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{encodedsecuritydesc=Value}, Rest);
decodedontknowtrade(Msg, [["350", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{encodedsecuritydesclen=Value}, Rest);
decodedontknowtrade(Msg, [["107", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{securitydesc=Value}, Rest);
decodedontknowtrade(Msg, [["349", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{encodedissuer=Value}, Rest);
decodedontknowtrade(Msg, [["348", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{encodedissuerlen=Value}, Rest);
decodedontknowtrade(Msg, [["106", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{issuer=Value}, Rest);
decodedontknowtrade(Msg, [["207", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{securityexchange=Value}, Rest);
decodedontknowtrade(Msg, [["223", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{couponrate=Value}, Rest);
decodedontknowtrade(Msg, [["231", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{contractmultiplier=Value}, Rest);
decodedontknowtrade(Msg, [["206", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{optattribute=Value}, Rest);
decodedontknowtrade(Msg, [["202", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{strikeprice=Value}, Rest);
decodedontknowtrade(Msg, [["201", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{putorcall=Value}, Rest);
decodedontknowtrade(Msg, [["205", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{maturityday=Value}, Rest);
decodedontknowtrade(Msg, [["200", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{maturitymonthyear=Value}, Rest);
decodedontknowtrade(Msg, [["167", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{securitytype=Value}, Rest);
decodedontknowtrade(Msg, [["22", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{idsource=Value}, Rest);
decodedontknowtrade(Msg, [["48", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{securityid=Value}, Rest);
decodedontknowtrade(Msg, [["65", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{symbolsfx=Value}, Rest);
decodedontknowtrade(Msg, [["55", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{symbol=Value}, Rest);
decodedontknowtrade(Msg, [["127", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{dkreason=Value}, Rest);
decodedontknowtrade(Msg, [["17", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{execid=Value}, Rest);
decodedontknowtrade(Msg, [["37", Value] | Rest]) -> 
    decodedontknowtrade(Msg#dontknowtrade{orderid=Value}, Rest);
decodedontknowtrade(Msg, [[_, Value] | Rest]) ->
    decodedontknowtrade(Msg,Rest).

decodequoterequest(Msg,[]) -> 
    Msg;
decodequoterequest(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{onbehalfofsendingtime=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{lastmsgseqnumprocessed=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{messageencoding=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{xmldata=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{xmldatalen=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{origsendingtime=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{sendingtime=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{possresend=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{possdupflag=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{delivertolocationid=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{delivertosubid=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{onbehalfoflocationid=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{onbehalfofsubid=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{targetlocationid=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{targetsubid=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{senderlocationid=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{sendersubid=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{msgseqnum=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{securedata=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{securedatalen=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{delivertocompid=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{onbehalfofcompid=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{targetcompid=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{sendercompid=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{msgtype=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{bodylength=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#quoterequest.header#header{beginstring=Value},
    decodequoterequest(Msg#quoterequest{header=Header},Rest);
decodequoterequest(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#quoterequest.trailer#trailer{checksum=Value},
    decodequoterequest(Msg#quoterequest{trailer=Trailer},Rest);
decodequoterequest(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#quoterequest.trailer#trailer{signature=Value},
    decodequoterequest(Msg#quoterequest{trailer=Trailer},Rest);
decodequoterequest(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#quoterequest.trailer#trailer{signaturelength=Value},
    decodequoterequest(Msg#quoterequest{trailer=Trailer},Rest);
decodequoterequest(Msg, [["131", Value] | Rest]) -> 
    decodequoterequest(Msg#quoterequest{quotereqid=Value}, Rest);
decodequoterequest(Msg, [[_, Value] | Rest]) ->
    decodequoterequest(Msg,Rest).

decodequote(Msg,[]) -> 
    Msg;
decodequote(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#quote.header#header{onbehalfofsendingtime=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#quote.header#header{lastmsgseqnumprocessed=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#quote.header#header{messageencoding=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#quote.header#header{xmldata=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#quote.header#header{xmldatalen=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#quote.header#header{origsendingtime=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#quote.header#header{sendingtime=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#quote.header#header{possresend=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#quote.header#header{possdupflag=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#quote.header#header{delivertolocationid=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#quote.header#header{delivertosubid=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#quote.header#header{onbehalfoflocationid=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#quote.header#header{onbehalfofsubid=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#quote.header#header{targetlocationid=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#quote.header#header{targetsubid=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#quote.header#header{senderlocationid=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#quote.header#header{sendersubid=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#quote.header#header{msgseqnum=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#quote.header#header{securedata=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#quote.header#header{securedatalen=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#quote.header#header{delivertocompid=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#quote.header#header{onbehalfofcompid=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#quote.header#header{targetcompid=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#quote.header#header{sendercompid=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#quote.header#header{msgtype=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#quote.header#header{bodylength=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#quote.header#header{beginstring=Value},
    decodequote(Msg#quote{header=Header},Rest);
decodequote(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#quote.trailer#trailer{checksum=Value},
    decodequote(Msg#quote{trailer=Trailer},Rest);
decodequote(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#quote.trailer#trailer{signature=Value},
    decodequote(Msg#quote{trailer=Trailer},Rest);
decodequote(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#quote.trailer#trailer{signaturelength=Value},
    decodequote(Msg#quote{trailer=Trailer},Rest);
decodequote(Msg, [["15", Value] | Rest]) -> 
    decodequote(Msg#quote{currency=Value}, Rest);
decodequote(Msg, [["192", Value] | Rest]) -> 
    decodequote(Msg#quote{orderqty2=Value}, Rest);
decodequote(Msg, [["193", Value] | Rest]) -> 
    decodequote(Msg#quote{futsettdate2=Value}, Rest);
decodequote(Msg, [["40", Value] | Rest]) -> 
    decodequote(Msg#quote{ordtype=Value}, Rest);
decodequote(Msg, [["64", Value] | Rest]) -> 
    decodequote(Msg#quote{futsettdate=Value}, Rest);
decodequote(Msg, [["60", Value] | Rest]) -> 
    decodequote(Msg#quote{transacttime=Value}, Rest);
decodequote(Msg, [["191", Value] | Rest]) -> 
    decodequote(Msg#quote{offerforwardpoints=Value}, Rest);
decodequote(Msg, [["189", Value] | Rest]) -> 
    decodequote(Msg#quote{bidforwardpoints=Value}, Rest);
decodequote(Msg, [["190", Value] | Rest]) -> 
    decodequote(Msg#quote{offerspotrate=Value}, Rest);
decodequote(Msg, [["188", Value] | Rest]) -> 
    decodequote(Msg#quote{bidspotrate=Value}, Rest);
decodequote(Msg, [["62", Value] | Rest]) -> 
    decodequote(Msg#quote{validuntiltime=Value}, Rest);
decodequote(Msg, [["135", Value] | Rest]) -> 
    decodequote(Msg#quote{offersize=Value}, Rest);
decodequote(Msg, [["134", Value] | Rest]) -> 
    decodequote(Msg#quote{bidsize=Value}, Rest);
decodequote(Msg, [["133", Value] | Rest]) -> 
    decodequote(Msg#quote{offerpx=Value}, Rest);
decodequote(Msg, [["132", Value] | Rest]) -> 
    decodequote(Msg#quote{bidpx=Value}, Rest);
decodequote(Msg, [["351", Value] | Rest]) -> 
    decodequote(Msg#quote{encodedsecuritydesc=Value}, Rest);
decodequote(Msg, [["350", Value] | Rest]) -> 
    decodequote(Msg#quote{encodedsecuritydesclen=Value}, Rest);
decodequote(Msg, [["107", Value] | Rest]) -> 
    decodequote(Msg#quote{securitydesc=Value}, Rest);
decodequote(Msg, [["349", Value] | Rest]) -> 
    decodequote(Msg#quote{encodedissuer=Value}, Rest);
decodequote(Msg, [["348", Value] | Rest]) -> 
    decodequote(Msg#quote{encodedissuerlen=Value}, Rest);
decodequote(Msg, [["106", Value] | Rest]) -> 
    decodequote(Msg#quote{issuer=Value}, Rest);
decodequote(Msg, [["207", Value] | Rest]) -> 
    decodequote(Msg#quote{securityexchange=Value}, Rest);
decodequote(Msg, [["223", Value] | Rest]) -> 
    decodequote(Msg#quote{couponrate=Value}, Rest);
decodequote(Msg, [["231", Value] | Rest]) -> 
    decodequote(Msg#quote{contractmultiplier=Value}, Rest);
decodequote(Msg, [["206", Value] | Rest]) -> 
    decodequote(Msg#quote{optattribute=Value}, Rest);
decodequote(Msg, [["202", Value] | Rest]) -> 
    decodequote(Msg#quote{strikeprice=Value}, Rest);
decodequote(Msg, [["201", Value] | Rest]) -> 
    decodequote(Msg#quote{putorcall=Value}, Rest);
decodequote(Msg, [["205", Value] | Rest]) -> 
    decodequote(Msg#quote{maturityday=Value}, Rest);
decodequote(Msg, [["200", Value] | Rest]) -> 
    decodequote(Msg#quote{maturitymonthyear=Value}, Rest);
decodequote(Msg, [["167", Value] | Rest]) -> 
    decodequote(Msg#quote{securitytype=Value}, Rest);
decodequote(Msg, [["22", Value] | Rest]) -> 
    decodequote(Msg#quote{idsource=Value}, Rest);
decodequote(Msg, [["48", Value] | Rest]) -> 
    decodequote(Msg#quote{securityid=Value}, Rest);
decodequote(Msg, [["65", Value] | Rest]) -> 
    decodequote(Msg#quote{symbolsfx=Value}, Rest);
decodequote(Msg, [["55", Value] | Rest]) -> 
    decodequote(Msg#quote{symbol=Value}, Rest);
decodequote(Msg, [["336", Value] | Rest]) -> 
    decodequote(Msg#quote{tradingsessionid=Value}, Rest);
decodequote(Msg, [["301", Value] | Rest]) -> 
    decodequote(Msg#quote{quoteresponselevel=Value}, Rest);
decodequote(Msg, [["117", Value] | Rest]) -> 
    decodequote(Msg#quote{quoteid=Value}, Rest);
decodequote(Msg, [["131", Value] | Rest]) -> 
    decodequote(Msg#quote{quotereqid=Value}, Rest);
decodequote(Msg, [[_, Value] | Rest]) ->
    decodequote(Msg,Rest).

decodesettlementinstructions(Msg,[]) -> 
    Msg;
decodesettlementinstructions(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{onbehalfofsendingtime=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{lastmsgseqnumprocessed=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{messageencoding=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{xmldata=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{xmldatalen=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{origsendingtime=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{sendingtime=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{possresend=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{possdupflag=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{delivertolocationid=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{delivertosubid=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{onbehalfoflocationid=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{onbehalfofsubid=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{targetlocationid=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{targetsubid=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{senderlocationid=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{sendersubid=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{msgseqnum=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{securedata=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{securedatalen=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{delivertocompid=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{onbehalfofcompid=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{targetcompid=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{sendercompid=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{msgtype=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{bodylength=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#settlementinstructions.header#header{beginstring=Value},
    decodesettlementinstructions(Msg#settlementinstructions{header=Header},Rest);
decodesettlementinstructions(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#settlementinstructions.trailer#trailer{checksum=Value},
    decodesettlementinstructions(Msg#settlementinstructions{trailer=Trailer},Rest);
decodesettlementinstructions(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#settlementinstructions.trailer#trailer{signature=Value},
    decodesettlementinstructions(Msg#settlementinstructions{trailer=Trailer},Rest);
decodesettlementinstructions(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#settlementinstructions.trailer#trailer{signaturelength=Value},
    decodesettlementinstructions(Msg#settlementinstructions{trailer=Trailer},Rest);
decodesettlementinstructions(Msg, [["187", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{cashsettlagentcontactphone=Value}, Rest);
decodesettlementinstructions(Msg, [["186", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{cashsettlagentcontactname=Value}, Rest);
decodesettlementinstructions(Msg, [["185", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{cashsettlagentacctname=Value}, Rest);
decodesettlementinstructions(Msg, [["184", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{cashsettlagentacctnum=Value}, Rest);
decodesettlementinstructions(Msg, [["183", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{cashsettlagentcode=Value}, Rest);
decodesettlementinstructions(Msg, [["182", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{cashsettlagentname=Value}, Rest);
decodesettlementinstructions(Msg, [["181", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{securitysettlagentcontactphone=Value}, Rest);
decodesettlementinstructions(Msg, [["180", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{securitysettlagentcontactname=Value}, Rest);
decodesettlementinstructions(Msg, [["179", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{securitysettlagentacctname=Value}, Rest);
decodesettlementinstructions(Msg, [["178", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{securitysettlagentacctnum=Value}, Rest);
decodesettlementinstructions(Msg, [["177", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{securitysettlagentcode=Value}, Rest);
decodesettlementinstructions(Msg, [["176", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{securitysettlagentname=Value}, Rest);
decodesettlementinstructions(Msg, [["175", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{settlinstcode=Value}, Rest);
decodesettlementinstructions(Msg, [["174", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{settlbrkrcode=Value}, Rest);
decodesettlementinstructions(Msg, [["173", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{settldepositorycode=Value}, Rest);
decodesettlementinstructions(Msg, [["172", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{settldeliverytype=Value}, Rest);
decodesettlementinstructions(Msg, [["171", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{standinstdbid=Value}, Rest);
decodesettlementinstructions(Msg, [["170", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{standinstdbname=Value}, Rest);
decodesettlementinstructions(Msg, [["169", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{standinstdbtype=Value}, Rest);
decodesettlementinstructions(Msg, [["76", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{execbroker=Value}, Rest);
decodesettlementinstructions(Msg, [["109", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{clientid=Value}, Rest);
decodesettlementinstructions(Msg, [["60", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{transacttime=Value}, Rest);
decodesettlementinstructions(Msg, [["168", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{effectivetime=Value}, Rest);
decodesettlementinstructions(Msg, [["167", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{securitytype=Value}, Rest);
decodesettlementinstructions(Msg, [["54", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{side=Value}, Rest);
decodesettlementinstructions(Msg, [["336", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{tradingsessionid=Value}, Rest);
decodesettlementinstructions(Msg, [["30", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{lastmkt=Value}, Rest);
decodesettlementinstructions(Msg, [["70", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{allocid=Value}, Rest);
decodesettlementinstructions(Msg, [["75", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{tradedate=Value}, Rest);
decodesettlementinstructions(Msg, [["166", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{settllocation=Value}, Rest);
decodesettlementinstructions(Msg, [["79", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{allocaccount=Value}, Rest);
decodesettlementinstructions(Msg, [["165", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{settlinstsource=Value}, Rest);
decodesettlementinstructions(Msg, [["160", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{settlinstmode=Value}, Rest);
decodesettlementinstructions(Msg, [["214", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{settlinstrefid=Value}, Rest);
decodesettlementinstructions(Msg, [["163", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{settlinsttranstype=Value}, Rest);
decodesettlementinstructions(Msg, [["162", Value] | Rest]) -> 
    decodesettlementinstructions(Msg#settlementinstructions{settlinstid=Value}, Rest);
decodesettlementinstructions(Msg, [[_, Value] | Rest]) ->
    decodesettlementinstructions(Msg,Rest).

decodemarketdatarequest(Msg,[]) -> 
    Msg;
decodemarketdatarequest(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{onbehalfofsendingtime=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{lastmsgseqnumprocessed=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{messageencoding=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{xmldata=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{xmldatalen=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{origsendingtime=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{sendingtime=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{possresend=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{possdupflag=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{delivertolocationid=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{delivertosubid=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{onbehalfoflocationid=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{onbehalfofsubid=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{targetlocationid=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{targetsubid=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{senderlocationid=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{sendersubid=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{msgseqnum=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{securedata=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{securedatalen=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{delivertocompid=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{onbehalfofcompid=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{targetcompid=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{sendercompid=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{msgtype=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{bodylength=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#marketdatarequest.header#header{beginstring=Value},
    decodemarketdatarequest(Msg#marketdatarequest{header=Header},Rest);
decodemarketdatarequest(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#marketdatarequest.trailer#trailer{checksum=Value},
    decodemarketdatarequest(Msg#marketdatarequest{trailer=Trailer},Rest);
decodemarketdatarequest(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#marketdatarequest.trailer#trailer{signature=Value},
    decodemarketdatarequest(Msg#marketdatarequest{trailer=Trailer},Rest);
decodemarketdatarequest(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#marketdatarequest.trailer#trailer{signaturelength=Value},
    decodemarketdatarequest(Msg#marketdatarequest{trailer=Trailer},Rest);
decodemarketdatarequest(Msg, [["266", Value] | Rest]) -> 
    decodemarketdatarequest(Msg#marketdatarequest{aggregatedbook=Value}, Rest);
decodemarketdatarequest(Msg, [["265", Value] | Rest]) -> 
    decodemarketdatarequest(Msg#marketdatarequest{mdupdatetype=Value}, Rest);
decodemarketdatarequest(Msg, [["264", Value] | Rest]) -> 
    decodemarketdatarequest(Msg#marketdatarequest{marketdepth=Value}, Rest);
decodemarketdatarequest(Msg, [["263", Value] | Rest]) -> 
    decodemarketdatarequest(Msg#marketdatarequest{subscriptionrequesttype=Value}, Rest);
decodemarketdatarequest(Msg, [["262", Value] | Rest]) -> 
    decodemarketdatarequest(Msg#marketdatarequest{mdreqid=Value}, Rest);
decodemarketdatarequest(Msg, [[_, Value] | Rest]) ->
    decodemarketdatarequest(Msg,Rest).

decodemarketdatasnapshotfullrefresh(Msg,[]) -> 
    Msg;
decodemarketdatasnapshotfullrefresh(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{onbehalfofsendingtime=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{lastmsgseqnumprocessed=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{messageencoding=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{xmldata=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{xmldatalen=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{origsendingtime=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{sendingtime=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{possresend=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{possdupflag=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{delivertolocationid=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{delivertosubid=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{onbehalfoflocationid=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{onbehalfofsubid=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{targetlocationid=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{targetsubid=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{senderlocationid=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{sendersubid=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{msgseqnum=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{securedata=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{securedatalen=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{delivertocompid=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{onbehalfofcompid=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{targetcompid=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{sendercompid=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{msgtype=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{bodylength=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#marketdatasnapshotfullrefresh.header#header{beginstring=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{header=Header},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#marketdatasnapshotfullrefresh.trailer#trailer{checksum=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{trailer=Trailer},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#marketdatasnapshotfullrefresh.trailer#trailer{signature=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{trailer=Trailer},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#marketdatasnapshotfullrefresh.trailer#trailer{signaturelength=Value},
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{trailer=Trailer},Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["387", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{totalvolumetraded=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["292", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{corporateaction=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["291", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{financialstatus=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["351", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{encodedsecuritydesc=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["350", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{encodedsecuritydesclen=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["107", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{securitydesc=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["349", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{encodedissuer=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["348", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{encodedissuerlen=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["106", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{issuer=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["207", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{securityexchange=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["223", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{couponrate=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["231", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{contractmultiplier=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["206", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{optattribute=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["202", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{strikeprice=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["201", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{putorcall=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["205", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{maturityday=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["200", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{maturitymonthyear=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["167", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{securitytype=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["22", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{idsource=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["48", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{securityid=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["65", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{symbolsfx=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["55", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{symbol=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [["262", Value] | Rest]) -> 
    decodemarketdatasnapshotfullrefresh(Msg#marketdatasnapshotfullrefresh{mdreqid=Value}, Rest);
decodemarketdatasnapshotfullrefresh(Msg, [[_, Value] | Rest]) ->
    decodemarketdatasnapshotfullrefresh(Msg,Rest).

decodemarketdataincrementalrefresh(Msg,[]) -> 
    Msg;
decodemarketdataincrementalrefresh(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{onbehalfofsendingtime=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{lastmsgseqnumprocessed=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{messageencoding=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{xmldata=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{xmldatalen=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{origsendingtime=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{sendingtime=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{possresend=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{possdupflag=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{delivertolocationid=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{delivertosubid=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{onbehalfoflocationid=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{onbehalfofsubid=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{targetlocationid=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{targetsubid=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{senderlocationid=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{sendersubid=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{msgseqnum=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{securedata=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{securedatalen=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{delivertocompid=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{onbehalfofcompid=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{targetcompid=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{sendercompid=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{msgtype=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{bodylength=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#marketdataincrementalrefresh.header#header{beginstring=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{header=Header},Rest);
decodemarketdataincrementalrefresh(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#marketdataincrementalrefresh.trailer#trailer{checksum=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{trailer=Trailer},Rest);
decodemarketdataincrementalrefresh(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#marketdataincrementalrefresh.trailer#trailer{signature=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{trailer=Trailer},Rest);
decodemarketdataincrementalrefresh(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#marketdataincrementalrefresh.trailer#trailer{signaturelength=Value},
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{trailer=Trailer},Rest);
decodemarketdataincrementalrefresh(Msg, [["262", Value] | Rest]) -> 
    decodemarketdataincrementalrefresh(Msg#marketdataincrementalrefresh{mdreqid=Value}, Rest);
decodemarketdataincrementalrefresh(Msg, [[_, Value] | Rest]) ->
    decodemarketdataincrementalrefresh(Msg,Rest).

decodemarketdatarequestreject(Msg,[]) -> 
    Msg;
decodemarketdatarequestreject(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{onbehalfofsendingtime=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{lastmsgseqnumprocessed=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{messageencoding=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{xmldata=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{xmldatalen=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{origsendingtime=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{sendingtime=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{possresend=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{possdupflag=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{delivertolocationid=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{delivertosubid=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{onbehalfoflocationid=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{onbehalfofsubid=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{targetlocationid=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{targetsubid=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{senderlocationid=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{sendersubid=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{msgseqnum=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{securedata=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{securedatalen=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{delivertocompid=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{onbehalfofcompid=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{targetcompid=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{sendercompid=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{msgtype=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{bodylength=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#marketdatarequestreject.header#header{beginstring=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{header=Header},Rest);
decodemarketdatarequestreject(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#marketdatarequestreject.trailer#trailer{checksum=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{trailer=Trailer},Rest);
decodemarketdatarequestreject(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#marketdatarequestreject.trailer#trailer{signature=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{trailer=Trailer},Rest);
decodemarketdatarequestreject(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#marketdatarequestreject.trailer#trailer{signaturelength=Value},
    decodemarketdatarequestreject(Msg#marketdatarequestreject{trailer=Trailer},Rest);
decodemarketdatarequestreject(Msg, [["355", Value] | Rest]) -> 
    decodemarketdatarequestreject(Msg#marketdatarequestreject{encodedtext=Value}, Rest);
decodemarketdatarequestreject(Msg, [["354", Value] | Rest]) -> 
    decodemarketdatarequestreject(Msg#marketdatarequestreject{encodedtextlen=Value}, Rest);
decodemarketdatarequestreject(Msg, [["58", Value] | Rest]) -> 
    decodemarketdatarequestreject(Msg#marketdatarequestreject{text=Value}, Rest);
decodemarketdatarequestreject(Msg, [["281", Value] | Rest]) -> 
    decodemarketdatarequestreject(Msg#marketdatarequestreject{mdreqrejreason=Value}, Rest);
decodemarketdatarequestreject(Msg, [["262", Value] | Rest]) -> 
    decodemarketdatarequestreject(Msg#marketdatarequestreject{mdreqid=Value}, Rest);
decodemarketdatarequestreject(Msg, [[_, Value] | Rest]) ->
    decodemarketdatarequestreject(Msg,Rest).

decodequotecancel(Msg,[]) -> 
    Msg;
decodequotecancel(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{onbehalfofsendingtime=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{lastmsgseqnumprocessed=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{messageencoding=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{xmldata=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{xmldatalen=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{origsendingtime=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{sendingtime=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{possresend=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{possdupflag=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{delivertolocationid=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{delivertosubid=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{onbehalfoflocationid=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{onbehalfofsubid=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{targetlocationid=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{targetsubid=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{senderlocationid=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{sendersubid=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{msgseqnum=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{securedata=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{securedatalen=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{delivertocompid=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{onbehalfofcompid=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{targetcompid=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{sendercompid=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{msgtype=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{bodylength=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#quotecancel.header#header{beginstring=Value},
    decodequotecancel(Msg#quotecancel{header=Header},Rest);
decodequotecancel(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#quotecancel.trailer#trailer{checksum=Value},
    decodequotecancel(Msg#quotecancel{trailer=Trailer},Rest);
decodequotecancel(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#quotecancel.trailer#trailer{signature=Value},
    decodequotecancel(Msg#quotecancel{trailer=Trailer},Rest);
decodequotecancel(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#quotecancel.trailer#trailer{signaturelength=Value},
    decodequotecancel(Msg#quotecancel{trailer=Trailer},Rest);
decodequotecancel(Msg, [["336", Value] | Rest]) -> 
    decodequotecancel(Msg#quotecancel{tradingsessionid=Value}, Rest);
decodequotecancel(Msg, [["301", Value] | Rest]) -> 
    decodequotecancel(Msg#quotecancel{quoteresponselevel=Value}, Rest);
decodequotecancel(Msg, [["298", Value] | Rest]) -> 
    decodequotecancel(Msg#quotecancel{quotecanceltype=Value}, Rest);
decodequotecancel(Msg, [["117", Value] | Rest]) -> 
    decodequotecancel(Msg#quotecancel{quoteid=Value}, Rest);
decodequotecancel(Msg, [["131", Value] | Rest]) -> 
    decodequotecancel(Msg#quotecancel{quotereqid=Value}, Rest);
decodequotecancel(Msg, [[_, Value] | Rest]) ->
    decodequotecancel(Msg,Rest).

decodequotestatusrequest(Msg,[]) -> 
    Msg;
decodequotestatusrequest(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{onbehalfofsendingtime=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{lastmsgseqnumprocessed=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{messageencoding=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{xmldata=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{xmldatalen=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{origsendingtime=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{sendingtime=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{possresend=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{possdupflag=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{delivertolocationid=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{delivertosubid=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{onbehalfoflocationid=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{onbehalfofsubid=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{targetlocationid=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{targetsubid=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{senderlocationid=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{sendersubid=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{msgseqnum=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{securedata=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{securedatalen=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{delivertocompid=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{onbehalfofcompid=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{targetcompid=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{sendercompid=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{msgtype=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{bodylength=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#quotestatusrequest.header#header{beginstring=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{header=Header},Rest);
decodequotestatusrequest(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#quotestatusrequest.trailer#trailer{checksum=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{trailer=Trailer},Rest);
decodequotestatusrequest(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#quotestatusrequest.trailer#trailer{signature=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{trailer=Trailer},Rest);
decodequotestatusrequest(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#quotestatusrequest.trailer#trailer{signaturelength=Value},
    decodequotestatusrequest(Msg#quotestatusrequest{trailer=Trailer},Rest);
decodequotestatusrequest(Msg, [["336", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{tradingsessionid=Value}, Rest);
decodequotestatusrequest(Msg, [["54", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{side=Value}, Rest);
decodequotestatusrequest(Msg, [["351", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{encodedsecuritydesc=Value}, Rest);
decodequotestatusrequest(Msg, [["350", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{encodedsecuritydesclen=Value}, Rest);
decodequotestatusrequest(Msg, [["107", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{securitydesc=Value}, Rest);
decodequotestatusrequest(Msg, [["349", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{encodedissuer=Value}, Rest);
decodequotestatusrequest(Msg, [["348", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{encodedissuerlen=Value}, Rest);
decodequotestatusrequest(Msg, [["106", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{issuer=Value}, Rest);
decodequotestatusrequest(Msg, [["207", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{securityexchange=Value}, Rest);
decodequotestatusrequest(Msg, [["223", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{couponrate=Value}, Rest);
decodequotestatusrequest(Msg, [["231", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{contractmultiplier=Value}, Rest);
decodequotestatusrequest(Msg, [["206", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{optattribute=Value}, Rest);
decodequotestatusrequest(Msg, [["202", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{strikeprice=Value}, Rest);
decodequotestatusrequest(Msg, [["201", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{putorcall=Value}, Rest);
decodequotestatusrequest(Msg, [["205", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{maturityday=Value}, Rest);
decodequotestatusrequest(Msg, [["200", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{maturitymonthyear=Value}, Rest);
decodequotestatusrequest(Msg, [["167", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{securitytype=Value}, Rest);
decodequotestatusrequest(Msg, [["22", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{idsource=Value}, Rest);
decodequotestatusrequest(Msg, [["48", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{securityid=Value}, Rest);
decodequotestatusrequest(Msg, [["65", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{symbolsfx=Value}, Rest);
decodequotestatusrequest(Msg, [["55", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{symbol=Value}, Rest);
decodequotestatusrequest(Msg, [["117", Value] | Rest]) -> 
    decodequotestatusrequest(Msg#quotestatusrequest{quoteid=Value}, Rest);
decodequotestatusrequest(Msg, [[_, Value] | Rest]) ->
    decodequotestatusrequest(Msg,Rest).

decodequoteacknowledgement(Msg,[]) -> 
    Msg;
decodequoteacknowledgement(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{onbehalfofsendingtime=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{lastmsgseqnumprocessed=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{messageencoding=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{xmldata=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{xmldatalen=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{origsendingtime=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{sendingtime=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{possresend=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{possdupflag=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{delivertolocationid=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{delivertosubid=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{onbehalfoflocationid=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{onbehalfofsubid=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{targetlocationid=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{targetsubid=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{senderlocationid=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{sendersubid=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{msgseqnum=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{securedata=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{securedatalen=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{delivertocompid=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{onbehalfofcompid=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{targetcompid=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{sendercompid=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{msgtype=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{bodylength=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#quoteacknowledgement.header#header{beginstring=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{header=Header},Rest);
decodequoteacknowledgement(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#quoteacknowledgement.trailer#trailer{checksum=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{trailer=Trailer},Rest);
decodequoteacknowledgement(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#quoteacknowledgement.trailer#trailer{signature=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{trailer=Trailer},Rest);
decodequoteacknowledgement(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#quoteacknowledgement.trailer#trailer{signaturelength=Value},
    decodequoteacknowledgement(Msg#quoteacknowledgement{trailer=Trailer},Rest);
decodequoteacknowledgement(Msg, [["58", Value] | Rest]) -> 
    decodequoteacknowledgement(Msg#quoteacknowledgement{text=Value}, Rest);
decodequoteacknowledgement(Msg, [["336", Value] | Rest]) -> 
    decodequoteacknowledgement(Msg#quoteacknowledgement{tradingsessionid=Value}, Rest);
decodequoteacknowledgement(Msg, [["301", Value] | Rest]) -> 
    decodequoteacknowledgement(Msg#quoteacknowledgement{quoteresponselevel=Value}, Rest);
decodequoteacknowledgement(Msg, [["300", Value] | Rest]) -> 
    decodequoteacknowledgement(Msg#quoteacknowledgement{quoterejectreason=Value}, Rest);
decodequoteacknowledgement(Msg, [["297", Value] | Rest]) -> 
    decodequoteacknowledgement(Msg#quoteacknowledgement{quoteackstatus=Value}, Rest);
decodequoteacknowledgement(Msg, [["117", Value] | Rest]) -> 
    decodequoteacknowledgement(Msg#quoteacknowledgement{quoteid=Value}, Rest);
decodequoteacknowledgement(Msg, [["131", Value] | Rest]) -> 
    decodequoteacknowledgement(Msg#quoteacknowledgement{quotereqid=Value}, Rest);
decodequoteacknowledgement(Msg, [[_, Value] | Rest]) ->
    decodequoteacknowledgement(Msg,Rest).

decodesecuritydefinitionrequest(Msg,[]) -> 
    Msg;
decodesecuritydefinitionrequest(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{onbehalfofsendingtime=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{lastmsgseqnumprocessed=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{messageencoding=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{xmldata=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{xmldatalen=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{origsendingtime=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{sendingtime=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{possresend=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{possdupflag=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{delivertolocationid=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{delivertosubid=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{onbehalfoflocationid=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{onbehalfofsubid=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{targetlocationid=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{targetsubid=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{senderlocationid=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{sendersubid=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{msgseqnum=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{securedata=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{securedatalen=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{delivertocompid=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{onbehalfofcompid=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{targetcompid=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{sendercompid=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{msgtype=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{bodylength=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#securitydefinitionrequest.header#header{beginstring=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{header=Header},Rest);
decodesecuritydefinitionrequest(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#securitydefinitionrequest.trailer#trailer{checksum=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{trailer=Trailer},Rest);
decodesecuritydefinitionrequest(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#securitydefinitionrequest.trailer#trailer{signature=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{trailer=Trailer},Rest);
decodesecuritydefinitionrequest(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#securitydefinitionrequest.trailer#trailer{signaturelength=Value},
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{trailer=Trailer},Rest);
decodesecuritydefinitionrequest(Msg, [["336", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{tradingsessionid=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["355", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{encodedtext=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["354", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{encodedtextlen=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["58", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{text=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["15", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{currency=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["351", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{encodedsecuritydesc=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["350", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{encodedsecuritydesclen=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["107", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{securitydesc=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["349", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{encodedissuer=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["348", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{encodedissuerlen=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["106", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{issuer=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["207", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{securityexchange=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["223", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{couponrate=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["231", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{contractmultiplier=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["206", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{optattribute=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["202", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{strikeprice=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["201", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{putorcall=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["205", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{maturityday=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["200", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{maturitymonthyear=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["167", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{securitytype=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["22", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{idsource=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["48", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{securityid=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["65", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{symbolsfx=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["55", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{symbol=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["321", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{securityrequesttype=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [["320", Value] | Rest]) -> 
    decodesecuritydefinitionrequest(Msg#securitydefinitionrequest{securityreqid=Value}, Rest);
decodesecuritydefinitionrequest(Msg, [[_, Value] | Rest]) ->
    decodesecuritydefinitionrequest(Msg,Rest).

decodesecuritydefinition(Msg,[]) -> 
    Msg;
decodesecuritydefinition(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{onbehalfofsendingtime=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{lastmsgseqnumprocessed=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{messageencoding=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{xmldata=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{xmldatalen=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{origsendingtime=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{sendingtime=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{possresend=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{possdupflag=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{delivertolocationid=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{delivertosubid=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{onbehalfoflocationid=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{onbehalfofsubid=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{targetlocationid=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{targetsubid=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{senderlocationid=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{sendersubid=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{msgseqnum=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{securedata=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{securedatalen=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{delivertocompid=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{onbehalfofcompid=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{targetcompid=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{sendercompid=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{msgtype=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{bodylength=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#securitydefinition.header#header{beginstring=Value},
    decodesecuritydefinition(Msg#securitydefinition{header=Header},Rest);
decodesecuritydefinition(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#securitydefinition.trailer#trailer{checksum=Value},
    decodesecuritydefinition(Msg#securitydefinition{trailer=Trailer},Rest);
decodesecuritydefinition(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#securitydefinition.trailer#trailer{signature=Value},
    decodesecuritydefinition(Msg#securitydefinition{trailer=Trailer},Rest);
decodesecuritydefinition(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#securitydefinition.trailer#trailer{signaturelength=Value},
    decodesecuritydefinition(Msg#securitydefinition{trailer=Trailer},Rest);
decodesecuritydefinition(Msg, [["355", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{encodedtext=Value}, Rest);
decodesecuritydefinition(Msg, [["354", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{encodedtextlen=Value}, Rest);
decodesecuritydefinition(Msg, [["58", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{text=Value}, Rest);
decodesecuritydefinition(Msg, [["336", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{tradingsessionid=Value}, Rest);
decodesecuritydefinition(Msg, [["15", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{currency=Value}, Rest);
decodesecuritydefinition(Msg, [["351", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{encodedsecuritydesc=Value}, Rest);
decodesecuritydefinition(Msg, [["350", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{encodedsecuritydesclen=Value}, Rest);
decodesecuritydefinition(Msg, [["107", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{securitydesc=Value}, Rest);
decodesecuritydefinition(Msg, [["349", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{encodedissuer=Value}, Rest);
decodesecuritydefinition(Msg, [["348", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{encodedissuerlen=Value}, Rest);
decodesecuritydefinition(Msg, [["106", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{issuer=Value}, Rest);
decodesecuritydefinition(Msg, [["207", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{securityexchange=Value}, Rest);
decodesecuritydefinition(Msg, [["223", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{couponrate=Value}, Rest);
decodesecuritydefinition(Msg, [["231", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{contractmultiplier=Value}, Rest);
decodesecuritydefinition(Msg, [["206", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{optattribute=Value}, Rest);
decodesecuritydefinition(Msg, [["202", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{strikeprice=Value}, Rest);
decodesecuritydefinition(Msg, [["201", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{putorcall=Value}, Rest);
decodesecuritydefinition(Msg, [["205", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{maturityday=Value}, Rest);
decodesecuritydefinition(Msg, [["200", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{maturitymonthyear=Value}, Rest);
decodesecuritydefinition(Msg, [["167", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{securitytype=Value}, Rest);
decodesecuritydefinition(Msg, [["22", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{idsource=Value}, Rest);
decodesecuritydefinition(Msg, [["48", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{securityid=Value}, Rest);
decodesecuritydefinition(Msg, [["65", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{symbolsfx=Value}, Rest);
decodesecuritydefinition(Msg, [["55", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{symbol=Value}, Rest);
decodesecuritydefinition(Msg, [["393", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{totalnumsecurities=Value}, Rest);
decodesecuritydefinition(Msg, [["323", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{securityresponsetype=Value}, Rest);
decodesecuritydefinition(Msg, [["322", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{securityresponseid=Value}, Rest);
decodesecuritydefinition(Msg, [["320", Value] | Rest]) -> 
    decodesecuritydefinition(Msg#securitydefinition{securityreqid=Value}, Rest);
decodesecuritydefinition(Msg, [[_, Value] | Rest]) ->
    decodesecuritydefinition(Msg,Rest).

decodesecuritystatusrequest(Msg,[]) -> 
    Msg;
decodesecuritystatusrequest(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{onbehalfofsendingtime=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{lastmsgseqnumprocessed=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{messageencoding=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{xmldata=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{xmldatalen=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{origsendingtime=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{sendingtime=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{possresend=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{possdupflag=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{delivertolocationid=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{delivertosubid=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{onbehalfoflocationid=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{onbehalfofsubid=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{targetlocationid=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{targetsubid=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{senderlocationid=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{sendersubid=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{msgseqnum=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{securedata=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{securedatalen=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{delivertocompid=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{onbehalfofcompid=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{targetcompid=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{sendercompid=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{msgtype=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{bodylength=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#securitystatusrequest.header#header{beginstring=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{header=Header},Rest);
decodesecuritystatusrequest(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#securitystatusrequest.trailer#trailer{checksum=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{trailer=Trailer},Rest);
decodesecuritystatusrequest(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#securitystatusrequest.trailer#trailer{signature=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{trailer=Trailer},Rest);
decodesecuritystatusrequest(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#securitystatusrequest.trailer#trailer{signaturelength=Value},
    decodesecuritystatusrequest(Msg#securitystatusrequest{trailer=Trailer},Rest);
decodesecuritystatusrequest(Msg, [["336", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{tradingsessionid=Value}, Rest);
decodesecuritystatusrequest(Msg, [["263", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{subscriptionrequesttype=Value}, Rest);
decodesecuritystatusrequest(Msg, [["15", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{currency=Value}, Rest);
decodesecuritystatusrequest(Msg, [["351", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{encodedsecuritydesc=Value}, Rest);
decodesecuritystatusrequest(Msg, [["350", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{encodedsecuritydesclen=Value}, Rest);
decodesecuritystatusrequest(Msg, [["107", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{securitydesc=Value}, Rest);
decodesecuritystatusrequest(Msg, [["349", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{encodedissuer=Value}, Rest);
decodesecuritystatusrequest(Msg, [["348", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{encodedissuerlen=Value}, Rest);
decodesecuritystatusrequest(Msg, [["106", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{issuer=Value}, Rest);
decodesecuritystatusrequest(Msg, [["207", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{securityexchange=Value}, Rest);
decodesecuritystatusrequest(Msg, [["223", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{couponrate=Value}, Rest);
decodesecuritystatusrequest(Msg, [["231", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{contractmultiplier=Value}, Rest);
decodesecuritystatusrequest(Msg, [["206", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{optattribute=Value}, Rest);
decodesecuritystatusrequest(Msg, [["202", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{strikeprice=Value}, Rest);
decodesecuritystatusrequest(Msg, [["201", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{putorcall=Value}, Rest);
decodesecuritystatusrequest(Msg, [["205", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{maturityday=Value}, Rest);
decodesecuritystatusrequest(Msg, [["200", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{maturitymonthyear=Value}, Rest);
decodesecuritystatusrequest(Msg, [["167", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{securitytype=Value}, Rest);
decodesecuritystatusrequest(Msg, [["22", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{idsource=Value}, Rest);
decodesecuritystatusrequest(Msg, [["48", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{securityid=Value}, Rest);
decodesecuritystatusrequest(Msg, [["65", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{symbolsfx=Value}, Rest);
decodesecuritystatusrequest(Msg, [["55", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{symbol=Value}, Rest);
decodesecuritystatusrequest(Msg, [["324", Value] | Rest]) -> 
    decodesecuritystatusrequest(Msg#securitystatusrequest{securitystatusreqid=Value}, Rest);
decodesecuritystatusrequest(Msg, [[_, Value] | Rest]) ->
    decodesecuritystatusrequest(Msg,Rest).

decodesecuritystatus(Msg,[]) -> 
    Msg;
decodesecuritystatus(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{onbehalfofsendingtime=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{lastmsgseqnumprocessed=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{messageencoding=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{xmldata=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{xmldatalen=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{origsendingtime=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{sendingtime=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{possresend=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{possdupflag=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{delivertolocationid=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{delivertosubid=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{onbehalfoflocationid=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{onbehalfofsubid=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{targetlocationid=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{targetsubid=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{senderlocationid=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{sendersubid=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{msgseqnum=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{securedata=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{securedatalen=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{delivertocompid=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{onbehalfofcompid=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{targetcompid=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{sendercompid=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{msgtype=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{bodylength=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#securitystatus.header#header{beginstring=Value},
    decodesecuritystatus(Msg#securitystatus{header=Header},Rest);
decodesecuritystatus(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#securitystatus.trailer#trailer{checksum=Value},
    decodesecuritystatus(Msg#securitystatus{trailer=Trailer},Rest);
decodesecuritystatus(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#securitystatus.trailer#trailer{signature=Value},
    decodesecuritystatus(Msg#securitystatus{trailer=Trailer},Rest);
decodesecuritystatus(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#securitystatus.trailer#trailer{signaturelength=Value},
    decodesecuritystatus(Msg#securitystatus{trailer=Trailer},Rest);
decodesecuritystatus(Msg, [["334", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{adjustment=Value}, Rest);
decodesecuritystatus(Msg, [["60", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{transacttime=Value}, Rest);
decodesecuritystatus(Msg, [["31", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{lastpx=Value}, Rest);
decodesecuritystatus(Msg, [["333", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{lowpx=Value}, Rest);
decodesecuritystatus(Msg, [["332", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{highpx=Value}, Rest);
decodesecuritystatus(Msg, [["331", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{sellvolume=Value}, Rest);
decodesecuritystatus(Msg, [["330", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{buyvolume=Value}, Rest);
decodesecuritystatus(Msg, [["329", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{duetorelated=Value}, Rest);
decodesecuritystatus(Msg, [["328", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{inviewofcommon=Value}, Rest);
decodesecuritystatus(Msg, [["327", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{haltreasonchar=Value}, Rest);
decodesecuritystatus(Msg, [["292", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{corporateaction=Value}, Rest);
decodesecuritystatus(Msg, [["291", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{financialstatus=Value}, Rest);
decodesecuritystatus(Msg, [["326", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{securitytradingstatus=Value}, Rest);
decodesecuritystatus(Msg, [["325", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{unsolicitedindicator=Value}, Rest);
decodesecuritystatus(Msg, [["336", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{tradingsessionid=Value}, Rest);
decodesecuritystatus(Msg, [["15", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{currency=Value}, Rest);
decodesecuritystatus(Msg, [["351", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{encodedsecuritydesc=Value}, Rest);
decodesecuritystatus(Msg, [["350", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{encodedsecuritydesclen=Value}, Rest);
decodesecuritystatus(Msg, [["107", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{securitydesc=Value}, Rest);
decodesecuritystatus(Msg, [["349", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{encodedissuer=Value}, Rest);
decodesecuritystatus(Msg, [["348", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{encodedissuerlen=Value}, Rest);
decodesecuritystatus(Msg, [["106", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{issuer=Value}, Rest);
decodesecuritystatus(Msg, [["207", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{securityexchange=Value}, Rest);
decodesecuritystatus(Msg, [["223", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{couponrate=Value}, Rest);
decodesecuritystatus(Msg, [["231", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{contractmultiplier=Value}, Rest);
decodesecuritystatus(Msg, [["206", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{optattribute=Value}, Rest);
decodesecuritystatus(Msg, [["202", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{strikeprice=Value}, Rest);
decodesecuritystatus(Msg, [["201", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{putorcall=Value}, Rest);
decodesecuritystatus(Msg, [["205", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{maturityday=Value}, Rest);
decodesecuritystatus(Msg, [["200", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{maturitymonthyear=Value}, Rest);
decodesecuritystatus(Msg, [["167", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{securitytype=Value}, Rest);
decodesecuritystatus(Msg, [["22", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{idsource=Value}, Rest);
decodesecuritystatus(Msg, [["48", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{securityid=Value}, Rest);
decodesecuritystatus(Msg, [["65", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{symbolsfx=Value}, Rest);
decodesecuritystatus(Msg, [["55", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{symbol=Value}, Rest);
decodesecuritystatus(Msg, [["324", Value] | Rest]) -> 
    decodesecuritystatus(Msg#securitystatus{securitystatusreqid=Value}, Rest);
decodesecuritystatus(Msg, [[_, Value] | Rest]) ->
    decodesecuritystatus(Msg,Rest).

decodetradingsessionstatusrequest(Msg,[]) -> 
    Msg;
decodetradingsessionstatusrequest(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{onbehalfofsendingtime=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{lastmsgseqnumprocessed=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{messageencoding=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{xmldata=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{xmldatalen=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{origsendingtime=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{sendingtime=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{possresend=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{possdupflag=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{delivertolocationid=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{delivertosubid=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{onbehalfoflocationid=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{onbehalfofsubid=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{targetlocationid=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{targetsubid=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{senderlocationid=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{sendersubid=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{msgseqnum=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{securedata=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{securedatalen=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{delivertocompid=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{onbehalfofcompid=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{targetcompid=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{sendercompid=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{msgtype=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{bodylength=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatusrequest.header#header{beginstring=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{header=Header},Rest);
decodetradingsessionstatusrequest(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#tradingsessionstatusrequest.trailer#trailer{checksum=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{trailer=Trailer},Rest);
decodetradingsessionstatusrequest(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#tradingsessionstatusrequest.trailer#trailer{signature=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{trailer=Trailer},Rest);
decodetradingsessionstatusrequest(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#tradingsessionstatusrequest.trailer#trailer{signaturelength=Value},
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{trailer=Trailer},Rest);
decodetradingsessionstatusrequest(Msg, [["263", Value] | Rest]) -> 
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{subscriptionrequesttype=Value}, Rest);
decodetradingsessionstatusrequest(Msg, [["339", Value] | Rest]) -> 
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{tradsesmode=Value}, Rest);
decodetradingsessionstatusrequest(Msg, [["338", Value] | Rest]) -> 
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{tradsesmethod=Value}, Rest);
decodetradingsessionstatusrequest(Msg, [["336", Value] | Rest]) -> 
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{tradingsessionid=Value}, Rest);
decodetradingsessionstatusrequest(Msg, [["335", Value] | Rest]) -> 
    decodetradingsessionstatusrequest(Msg#tradingsessionstatusrequest{tradsesreqid=Value}, Rest);
decodetradingsessionstatusrequest(Msg, [[_, Value] | Rest]) ->
    decodetradingsessionstatusrequest(Msg,Rest).

decodetradingsessionstatus(Msg,[]) -> 
    Msg;
decodetradingsessionstatus(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{onbehalfofsendingtime=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{lastmsgseqnumprocessed=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{messageencoding=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{xmldata=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{xmldatalen=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{origsendingtime=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{sendingtime=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{possresend=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{possdupflag=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{delivertolocationid=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{delivertosubid=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{onbehalfoflocationid=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{onbehalfofsubid=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{targetlocationid=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{targetsubid=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{senderlocationid=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{sendersubid=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{msgseqnum=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{securedata=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{securedatalen=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{delivertocompid=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{onbehalfofcompid=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{targetcompid=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{sendercompid=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{msgtype=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{bodylength=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#tradingsessionstatus.header#header{beginstring=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{header=Header},Rest);
decodetradingsessionstatus(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#tradingsessionstatus.trailer#trailer{checksum=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{trailer=Trailer},Rest);
decodetradingsessionstatus(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#tradingsessionstatus.trailer#trailer{signature=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{trailer=Trailer},Rest);
decodetradingsessionstatus(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#tradingsessionstatus.trailer#trailer{signaturelength=Value},
    decodetradingsessionstatus(Msg#tradingsessionstatus{trailer=Trailer},Rest);
decodetradingsessionstatus(Msg, [["355", Value] | Rest]) -> 
    decodetradingsessionstatus(Msg#tradingsessionstatus{encodedtext=Value}, Rest);
decodetradingsessionstatus(Msg, [["354", Value] | Rest]) -> 
    decodetradingsessionstatus(Msg#tradingsessionstatus{encodedtextlen=Value}, Rest);
decodetradingsessionstatus(Msg, [["58", Value] | Rest]) -> 
    decodetradingsessionstatus(Msg#tradingsessionstatus{text=Value}, Rest);
decodetradingsessionstatus(Msg, [["387", Value] | Rest]) -> 
    decodetradingsessionstatus(Msg#tradingsessionstatus{totalvolumetraded=Value}, Rest);
decodetradingsessionstatus(Msg, [["345", Value] | Rest]) -> 
    decodetradingsessionstatus(Msg#tradingsessionstatus{tradsesendtime=Value}, Rest);
decodetradingsessionstatus(Msg, [["344", Value] | Rest]) -> 
    decodetradingsessionstatus(Msg#tradingsessionstatus{tradsesclosetime=Value}, Rest);
decodetradingsessionstatus(Msg, [["343", Value] | Rest]) -> 
    decodetradingsessionstatus(Msg#tradingsessionstatus{tradsespreclosetime=Value}, Rest);
decodetradingsessionstatus(Msg, [["342", Value] | Rest]) -> 
    decodetradingsessionstatus(Msg#tradingsessionstatus{tradsesopentime=Value}, Rest);
decodetradingsessionstatus(Msg, [["341", Value] | Rest]) -> 
    decodetradingsessionstatus(Msg#tradingsessionstatus{tradsesstarttime=Value}, Rest);
decodetradingsessionstatus(Msg, [["340", Value] | Rest]) -> 
    decodetradingsessionstatus(Msg#tradingsessionstatus{tradsesstatus=Value}, Rest);
decodetradingsessionstatus(Msg, [["325", Value] | Rest]) -> 
    decodetradingsessionstatus(Msg#tradingsessionstatus{unsolicitedindicator=Value}, Rest);
decodetradingsessionstatus(Msg, [["339", Value] | Rest]) -> 
    decodetradingsessionstatus(Msg#tradingsessionstatus{tradsesmode=Value}, Rest);
decodetradingsessionstatus(Msg, [["338", Value] | Rest]) -> 
    decodetradingsessionstatus(Msg#tradingsessionstatus{tradsesmethod=Value}, Rest);
decodetradingsessionstatus(Msg, [["336", Value] | Rest]) -> 
    decodetradingsessionstatus(Msg#tradingsessionstatus{tradingsessionid=Value}, Rest);
decodetradingsessionstatus(Msg, [["335", Value] | Rest]) -> 
    decodetradingsessionstatus(Msg#tradingsessionstatus{tradsesreqid=Value}, Rest);
decodetradingsessionstatus(Msg, [[_, Value] | Rest]) ->
    decodetradingsessionstatus(Msg,Rest).

decodemassquote(Msg,[]) -> 
    Msg;
decodemassquote(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{onbehalfofsendingtime=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{lastmsgseqnumprocessed=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{messageencoding=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{xmldata=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{xmldatalen=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{origsendingtime=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{sendingtime=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{possresend=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{possdupflag=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{delivertolocationid=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{delivertosubid=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{onbehalfoflocationid=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{onbehalfofsubid=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{targetlocationid=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{targetsubid=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{senderlocationid=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{sendersubid=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{msgseqnum=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{securedata=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{securedatalen=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{delivertocompid=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{onbehalfofcompid=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{targetcompid=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{sendercompid=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{msgtype=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{bodylength=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#massquote.header#header{beginstring=Value},
    decodemassquote(Msg#massquote{header=Header},Rest);
decodemassquote(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#massquote.trailer#trailer{checksum=Value},
    decodemassquote(Msg#massquote{trailer=Trailer},Rest);
decodemassquote(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#massquote.trailer#trailer{signature=Value},
    decodemassquote(Msg#massquote{trailer=Trailer},Rest);
decodemassquote(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#massquote.trailer#trailer{signaturelength=Value},
    decodemassquote(Msg#massquote{trailer=Trailer},Rest);
decodemassquote(Msg, [["294", Value] | Rest]) -> 
    decodemassquote(Msg#massquote{defoffersize=Value}, Rest);
decodemassquote(Msg, [["293", Value] | Rest]) -> 
    decodemassquote(Msg#massquote{defbidsize=Value}, Rest);
decodemassquote(Msg, [["301", Value] | Rest]) -> 
    decodemassquote(Msg#massquote{quoteresponselevel=Value}, Rest);
decodemassquote(Msg, [["117", Value] | Rest]) -> 
    decodemassquote(Msg#massquote{quoteid=Value}, Rest);
decodemassquote(Msg, [["131", Value] | Rest]) -> 
    decodemassquote(Msg#massquote{quotereqid=Value}, Rest);
decodemassquote(Msg, [[_, Value] | Rest]) ->
    decodemassquote(Msg,Rest).

decodebusinessmessagereject(Msg,[]) -> 
    Msg;
decodebusinessmessagereject(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{onbehalfofsendingtime=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{lastmsgseqnumprocessed=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{messageencoding=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{xmldata=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{xmldatalen=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{origsendingtime=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{sendingtime=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{possresend=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{possdupflag=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{delivertolocationid=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{delivertosubid=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{onbehalfoflocationid=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{onbehalfofsubid=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{targetlocationid=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{targetsubid=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{senderlocationid=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{sendersubid=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{msgseqnum=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{securedata=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{securedatalen=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{delivertocompid=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{onbehalfofcompid=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{targetcompid=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{sendercompid=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{msgtype=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{bodylength=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#businessmessagereject.header#header{beginstring=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{header=Header},Rest);
decodebusinessmessagereject(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#businessmessagereject.trailer#trailer{checksum=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{trailer=Trailer},Rest);
decodebusinessmessagereject(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#businessmessagereject.trailer#trailer{signature=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{trailer=Trailer},Rest);
decodebusinessmessagereject(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#businessmessagereject.trailer#trailer{signaturelength=Value},
    decodebusinessmessagereject(Msg#businessmessagereject{trailer=Trailer},Rest);
decodebusinessmessagereject(Msg, [["355", Value] | Rest]) -> 
    decodebusinessmessagereject(Msg#businessmessagereject{encodedtext=Value}, Rest);
decodebusinessmessagereject(Msg, [["354", Value] | Rest]) -> 
    decodebusinessmessagereject(Msg#businessmessagereject{encodedtextlen=Value}, Rest);
decodebusinessmessagereject(Msg, [["58", Value] | Rest]) -> 
    decodebusinessmessagereject(Msg#businessmessagereject{text=Value}, Rest);
decodebusinessmessagereject(Msg, [["380", Value] | Rest]) -> 
    decodebusinessmessagereject(Msg#businessmessagereject{businessrejectreason=Value}, Rest);
decodebusinessmessagereject(Msg, [["379", Value] | Rest]) -> 
    decodebusinessmessagereject(Msg#businessmessagereject{businessrejectrefid=Value}, Rest);
decodebusinessmessagereject(Msg, [["372", Value] | Rest]) -> 
    decodebusinessmessagereject(Msg#businessmessagereject{refmsgtype=Value}, Rest);
decodebusinessmessagereject(Msg, [["45", Value] | Rest]) -> 
    decodebusinessmessagereject(Msg#businessmessagereject{refseqnum=Value}, Rest);
decodebusinessmessagereject(Msg, [[_, Value] | Rest]) ->
    decodebusinessmessagereject(Msg,Rest).

decodebidrequest(Msg,[]) -> 
    Msg;
decodebidrequest(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{onbehalfofsendingtime=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{lastmsgseqnumprocessed=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{messageencoding=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{xmldata=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{xmldatalen=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{origsendingtime=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{sendingtime=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{possresend=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{possdupflag=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{delivertolocationid=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{delivertosubid=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{onbehalfoflocationid=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{onbehalfofsubid=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{targetlocationid=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{targetsubid=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{senderlocationid=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{sendersubid=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{msgseqnum=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{securedata=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{securedatalen=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{delivertocompid=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{onbehalfofcompid=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{targetcompid=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{sendercompid=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{msgtype=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{bodylength=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#bidrequest.header#header{beginstring=Value},
    decodebidrequest(Msg#bidrequest{header=Header},Rest);
decodebidrequest(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#bidrequest.trailer#trailer{checksum=Value},
    decodebidrequest(Msg#bidrequest{trailer=Trailer},Rest);
decodebidrequest(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#bidrequest.trailer#trailer{signature=Value},
    decodebidrequest(Msg#bidrequest{trailer=Trailer},Rest);
decodebidrequest(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#bidrequest.trailer#trailer{signaturelength=Value},
    decodebidrequest(Msg#bidrequest{trailer=Trailer},Rest);
decodebidrequest(Msg, [["355", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{encodedtext=Value}, Rest);
decodebidrequest(Msg, [["354", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{encodedtextlen=Value}, Rest);
decodebidrequest(Msg, [["58", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{text=Value}, Rest);
decodebidrequest(Msg, [["443", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{striketime=Value}, Rest);
decodebidrequest(Msg, [["419", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{basispxtype=Value}, Rest);
decodebidrequest(Msg, [["418", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{tradetype=Value}, Rest);
decodebidrequest(Msg, [["75", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{tradedate=Value}, Rest);
decodebidrequest(Msg, [["417", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{numbidders=Value}, Rest);
decodebidrequest(Msg, [["121", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{forexreq=Value}, Rest);
decodebidrequest(Msg, [["416", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{inctaxind=Value}, Rest);
decodebidrequest(Msg, [["415", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{progperiodinterval=Value}, Rest);
decodebidrequest(Msg, [["414", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{progrptreqs=Value}, Rest);
decodebidrequest(Msg, [["413", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{crosspercent=Value}, Rest);
decodebidrequest(Msg, [["412", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{outmaincntryuindex=Value}, Rest);
decodebidrequest(Msg, [["411", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{exchangeforphysical=Value}, Rest);
decodebidrequest(Msg, [["410", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{wtaverageliquidity=Value}, Rest);
decodebidrequest(Msg, [["409", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{liquidityindtype=Value}, Rest);
decodebidrequest(Msg, [["397", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{sidevalue2=Value}, Rest);
decodebidrequest(Msg, [["396", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{sidevalue1=Value}, Rest);
decodebidrequest(Msg, [["15", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{currency=Value}, Rest);
decodebidrequest(Msg, [["395", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{numtickets=Value}, Rest);
decodebidrequest(Msg, [["394", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{bidtype=Value}, Rest);
decodebidrequest(Msg, [["393", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{totalnumsecurities=Value}, Rest);
decodebidrequest(Msg, [["392", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{listname=Value}, Rest);
decodebidrequest(Msg, [["374", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{bidrequesttranstype=Value}, Rest);
decodebidrequest(Msg, [["391", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{clientbidid=Value}, Rest);
decodebidrequest(Msg, [["390", Value] | Rest]) -> 
    decodebidrequest(Msg#bidrequest{bidid=Value}, Rest);
decodebidrequest(Msg, [[_, Value] | Rest]) ->
    decodebidrequest(Msg,Rest).

decodebidresponse(Msg,[]) -> 
    Msg;
decodebidresponse(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{onbehalfofsendingtime=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{lastmsgseqnumprocessed=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{messageencoding=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{xmldata=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{xmldatalen=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{origsendingtime=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{sendingtime=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{possresend=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{possdupflag=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{delivertolocationid=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{delivertosubid=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{onbehalfoflocationid=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{onbehalfofsubid=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{targetlocationid=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{targetsubid=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{senderlocationid=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{sendersubid=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{msgseqnum=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{securedata=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{securedatalen=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{delivertocompid=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{onbehalfofcompid=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{targetcompid=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{sendercompid=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{msgtype=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{bodylength=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#bidresponse.header#header{beginstring=Value},
    decodebidresponse(Msg#bidresponse{header=Header},Rest);
decodebidresponse(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#bidresponse.trailer#trailer{checksum=Value},
    decodebidresponse(Msg#bidresponse{trailer=Trailer},Rest);
decodebidresponse(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#bidresponse.trailer#trailer{signature=Value},
    decodebidresponse(Msg#bidresponse{trailer=Trailer},Rest);
decodebidresponse(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#bidresponse.trailer#trailer{signaturelength=Value},
    decodebidresponse(Msg#bidresponse{trailer=Trailer},Rest);
decodebidresponse(Msg, [["391", Value] | Rest]) -> 
    decodebidresponse(Msg#bidresponse{clientbidid=Value}, Rest);
decodebidresponse(Msg, [["390", Value] | Rest]) -> 
    decodebidresponse(Msg#bidresponse{bidid=Value}, Rest);
decodebidresponse(Msg, [[_, Value] | Rest]) ->
    decodebidresponse(Msg,Rest).

decodeliststrikeprice(Msg,[]) -> 
    Msg;
decodeliststrikeprice(Msg, [["370", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{onbehalfofsendingtime=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["369", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{lastmsgseqnumprocessed=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["347", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{messageencoding=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["213", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{xmldata=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["212", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{xmldatalen=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["122", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{origsendingtime=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["52", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{sendingtime=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["97", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{possresend=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["43", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{possdupflag=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["145", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{delivertolocationid=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["129", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{delivertosubid=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["144", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{onbehalfoflocationid=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["116", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{onbehalfofsubid=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["143", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{targetlocationid=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["57", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{targetsubid=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["142", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{senderlocationid=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["50", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{sendersubid=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["34", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{msgseqnum=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["91", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{securedata=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["90", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{securedatalen=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["128", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{delivertocompid=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["115", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{onbehalfofcompid=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["56", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{targetcompid=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["49", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{sendercompid=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["35", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{msgtype=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["9", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{bodylength=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["8", Value] | Rest]) -> 
    Header = Msg#liststrikeprice.header#header{beginstring=Value},
    decodeliststrikeprice(Msg#liststrikeprice{header=Header},Rest);
decodeliststrikeprice(Msg, [["10", Value] | Rest]) -> 
    Trailer = Msg#liststrikeprice.trailer#trailer{checksum=Value},
    decodeliststrikeprice(Msg#liststrikeprice{trailer=Trailer},Rest);
decodeliststrikeprice(Msg, [["89", Value] | Rest]) -> 
    Trailer = Msg#liststrikeprice.trailer#trailer{signature=Value},
    decodeliststrikeprice(Msg#liststrikeprice{trailer=Trailer},Rest);
decodeliststrikeprice(Msg, [["93", Value] | Rest]) -> 
    Trailer = Msg#liststrikeprice.trailer#trailer{signaturelength=Value},
    decodeliststrikeprice(Msg#liststrikeprice{trailer=Trailer},Rest);
decodeliststrikeprice(Msg, [["422", Value] | Rest]) -> 
    decodeliststrikeprice(Msg#liststrikeprice{totnostrikes=Value}, Rest);
decodeliststrikeprice(Msg, [["66", Value] | Rest]) -> 
    decodeliststrikeprice(Msg#liststrikeprice{listid=Value}, Rest);
decodeliststrikeprice(Msg, [[_, Value] | Rest]) ->
    decodeliststrikeprice(Msg,Rest).


