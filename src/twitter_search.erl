-module(twitter_search).
-author("Yuce Tekol").

-export([get/3]).

-include("twitter.hrl").
-include("util.hrl").


get(Api, tweets, Args) ->
    Path = "search/tweets",
    {ok, Data} = twitter_rest:get(Api, Path, Args),
    {ok, twitter_rest:make_timeline(Api, Path, Args, Data, "statuses")}.
