-module(twitter_statuses).
-author("Yuce Tekol").

-export([get/3, get/4, post/3, post/4]).

-include("twitter.hrl").


get(Api, retweeters_ids, Args) ->
    twitter_rest:make_get_cursor(Api, "statuses/retweeters/ids", Args, "ids");

get(Api, oembed, Args) ->
    twitter_rest:get(Api, "statuses/oembed", Args);

get(Api, lookup, Args) ->
    twitter_rest:get(Api, "statuses/lookup", Args);

get(Api, Path, Args) ->
    NewPath = lists:concat(["statuses/", Path]),
    twitter_rest:make_get_timeline(Api, NewPath, Args, "").

get(Api, retweets, Id, Args) ->
    twitter_rest:get(Api, lists:concat(["statuses/retweets/", Id]), Args);

get(Api, show, Id, Args) ->
    twitter_rest:get(Api, lists:concat(["statuses/show/", Id]), Args).


post(Api, Path, Args) ->
    NewPath = lists:concat(["statuses/", Path]),
    twitter_rest:post(Api, NewPath, Args).

post(Api, destroy, Id, Args) ->
    twitter_rest:post(Api, lists:concat(["statuses/destroy/", Id]), Args).
