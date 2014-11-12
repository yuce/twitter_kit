-module(twitter_search).
-author("Yuce Tekol").

-export([get/3]).

-include("twitter.hrl").
-include("util.hrl").


get(Api, tweets, Args) ->
    twitter_rest:make_get_timeline(Api, "search/tweets", Args, "statuses").
