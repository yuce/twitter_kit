-module(twitter_friends).
-author("Yuce Tekol").

-export([get/3]).

-include("twitter.hrl").


get(Api, {ids}, Args) ->
    Path = "friends/ids",
    {ok, Items} = twitter_rest:get(Api, Path, Args),
    {ok, twitter_rest:make_cursor(Api, Path, Args, Items, "ids")};

get(Api, {list}, Args) ->
    Path = "friends/list",
    {ok, Items} = twitter_rest:get(Api, Path, Args),
    {ok, twitter_rest:make_cursor(Api, Path, Args, Items, "users")}.

