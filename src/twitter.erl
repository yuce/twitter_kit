-module(twitter).
-author("Yuce Tekol").

-export([new/1, get/3, post/3]).
-export([prev/1, next/1]).

-include("twitter.hrl").
-include("def.hrl").


-spec new(#oauth{}) -> #twitter{}.

new(Auth) ->
     #twitter{auth = Auth}.


%% API Helpers

%% statuses/*

get(Api, {statuses, Path}, Args) when Path == mentions_timeline;
                                      Path == user_timeline;
                                      Path == home_timeline;
                                      Path == retweets_of_me ->
    NewPath = lists:concat(["statuses/", Path]),
    twitter_rest:make_get_timeline(Api, NewPath, Args, "");

get(Api, {statuses, oembed}, Args) ->
    twitter_rest:get(Api, "statuses/oembed", Args);

get(Api, {statuses, lookup}, Args) ->
    twitter_rest:get(Api, "statuses/lookup", Args);

get(Api, {statuses, retweeters, ids}, Args) ->
    twitter_rest:make_get_cursor(Api, "statuses/retweeters/ids", Args, "ids");

get(Api, {statuses, retweets, Id}, Args) ->
    twitter_rest:get(Api, lists:concat(["statuses/retweets/", Id]), Args);

get(Api, {statuses, show, Id}, Args) ->
    twitter_rest:get(Api, lists:concat(["statuses/show/", Id]), Args).

post(Api, {statuses, update}, Args) ->
    twitter_rest:post(Api, "statuses/update", Args);

post(Api, {statuses, destroy, Id}, Args) ->
    twitter_rest:post(Api, lists:concat(["statuses/destroy/", Id]), Args);

post(Api, {statuses, retweet, Id}, Args) ->
    twitter_rest:post(Api, lists:concat(["statuses/retweet/", Id]), Args).


prev(X) ->
    twitter_rest:prev(X).

next(X) ->
    twitter_rest:next(X).
