-module(twitter_statuses_).
-author("Yuce Tekol").

-export([get/3, get/4, post/3, post/4]).

-include("twitter.hrl").


get(Api, oembed, Args) ->
    twitter_rest:get(Api, "statuses/oembed", Args);

get(Api, lookup, Args) ->
    twitter_rest:get(Api, "statuses/lookup", Args);

get(Api, Path, Args) when Path == mentions_timeline;
                          Path == user_timeline;
                          Path == home_timeline;
                          Path == retweets_of_me ->
    NewPath = lists:concat(["statuses/", Path]),
    twitter_rest:make_get_timeline(Api, NewPath, Args, "").

get(Api, retweeters, ids, Args) ->
    twitter_rest:make_get_cursor(Api, "statuses/retweeters/ids", Args, "ids");

get(Api, retweets, Id, Args) ->
    twitter_rest:get(Api, lists:concat(["statuses/retweets/", Id]), Args);

get(Api, show, Id, Args) ->
    twitter_rest:get(Api, lists:concat(["statuses/show/", Id]), Args).


post(Api, update, Args) ->
    twitter_rest:post(Api, "statuses/update", Args).

post(Api, destroy, Id, Args) ->
    twitter_rest:post(Api, lists:concat(["statuses/destroy/", Id]), Args);

post(Api, retweet, Id, Args) ->
    twitter_rest:post(Api, lists:concat(["statuses/retweet/", Id]), Args).