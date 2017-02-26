
-define(DBG(Str,Args), error_logger:info_msg("~w (line ~w): " ++ Str ++ "~n", [?MODULE, ?LINE | Args])).
