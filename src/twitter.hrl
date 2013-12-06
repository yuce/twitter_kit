%%%-------------------------------------------------------------------
%%% @author Yuce
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Ara 2013 03:44
%%%-------------------------------------------------------------------
-author("Yuce").

-record(twitter, {
  auth = nil,
  format = json,
  domain = "api.twitter.com",
  secure = true,
  api_version = "1.1"
}).
