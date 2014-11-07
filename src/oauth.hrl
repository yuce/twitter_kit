%%%-------------------------------------------------------------------
%%% @author Yuce
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Ara 2013 00:46
%%%-------------------------------------------------------------------
-author("Yuce").

-record(oauth, {
  token = "",
  token_secret = "",
  consumer_key = "",
  consumer_secret = ""
}).
