-module(fix_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("fix_messages.hrl").
-include_lib("fix_macros.hrl").

from_fix_to_heartbeat_test() ->
    Msg = fix_parser:decode("8=FIX.4.29=5335=034=249=BANZAI52=20100808-09:10:43.11056=EXEC112=1234510=163"),
    ?DBG("heartbeat = ~p ~n", [Msg]),
    ok.

