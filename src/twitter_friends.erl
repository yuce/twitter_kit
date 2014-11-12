-module(twitter_friends).
-author("Yuce Tekol").

-export([get/3]).

-include("twitter.hrl").


get(Api, ids, Args) ->
    twitter_rest:make_get_cursor(Api, "friends/ids", Args, "ids");

get(Api, list, Args) ->
    twitter_rest:make_get_cursor(Api, "friends/list", Args, "users").
