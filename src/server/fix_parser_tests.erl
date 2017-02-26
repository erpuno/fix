-module(fix_parser_tests).
-include_lib("eunit/include/eunit.hrl").
-include("fix_messages.hrl").
-compile(export_all).

heartbeat_decode_test() ->
    B = "8=FIX.4.29=5335=034=249=BANZAI52=20100808-09:10:43.11056=EXEC10=163",
    A = #heartbeat{trailer=#trailer{checksum="163"},
                   header=#header{beginstring="FIX.4.2", bodylength="53", msgtype="0",
                                  msgseqnum="2", sendercompid="BANZAI",sendingtime="20100808-09:10:43.110",
                                  targetcompid="EXEC"}},
    io:format("A: ~p",[A]),
    io:format("B: ~p",[fix_parser:decode(B)]),
    ?assertEqual(A, fix_parser:decode(B)).

%heartbeat_encode_test() ->
%    A = #heartbeat{trailer=#trailer{checksum="163"},
%                   header=#header{beginstring="FIX.4.2", bodylength="53", msgtype="0", msgseqnum="2",
%                    sendercompid="BANZAI", sendingtime="20100808-09:10:43.110", targetcompid="EXEC"}},
%    B = "8=FIX.4.29=5335=034=249=BANZAI52=20100808-09:10:43.11056=EXEC10=163",
%    io:format("A: ~p",[fix_parser:encode(A)]),
%    io:format("B: ~p",[B]),
%    ?assertEqual(fix_parser:encode(A), B).

logon_decode_test() ->
    Msg = "8=FIX.4.29=6535=A34=949=BANZAI52=20101001-15:55:32.50456=EXEC98=0108=3010=212",
    Logon = #logon{ trailer = #trailer{checksum = "212"},
                    heartbtint = "30", encryptmethod = "0",
                    header = #header{beginstring = "FIX.4.2", bodylength = "65", msgtype = "A",
                                     msgseqnum = "9", sendercompid = "BANZAI",
                                     sendingtime = "20101001-15:55:32.504",
                                     targetcompid = "EXEC"} },
    ?assertEqual(fix_parser:decode(Msg), Logon).

logon_pref() -> logon_pref(0).
logon_pref(100) -> ok;
logon_pref(Int) ->
    fix_parser:decode("8=FIX.4.29=6535=A34=149=BANZAI52=20100806-16:16:57.01156=EXEC98=0108=3010=214"),
    logon_pref(Int + 1).
