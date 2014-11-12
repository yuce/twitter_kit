-module(twitter_statuses).
-author("Yuce Tekol").

-export([get/3]).

-include("twitter.hrl").


get(Api, {retweeters, ids}, Args) ->
    Path = "statuses/retweeters/ids",
    {ok, Items} = twitter_rest:get(Api, Path, Args),
    {ok, twitter_rest:make_cursor(Api, Path, Args, Items, "ids")};

get(Api, {retweets, Id}, Args) ->
    twitter_rest:get(Api, lists:concat(["statuses/retweets/", Id]), Args);

get(Api, {show, Id}, Args) ->
    twitter_rest:get(Api, lists:concat(["statuses/show/", Id]), Args);

get(Api, {oembed}, Args) ->
    twitter_rest:get(Api, "statuses/oembed", Args);

get(Api, {lookup}, Args) ->
    twitter_rest:get(Api, "statuses/lookup", Args);

get(Api, {Path}, Args) ->
    NewPath = lists:concat(["statuses/", Path]),
    {ok, Items} = twitter_rest:get(Api, NewPath, Args),
    {ok, twitter_rest:make_timeline(Api, NewPath, Args, Items, "")}.
