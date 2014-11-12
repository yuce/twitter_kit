#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Search tweets.
%% Usage: escript search_tweets.escript [query]

-include("../src/twitter.hrl").

main(Args) ->
    Query = case Args of
        [] -> "erlang";
        [H|_] -> H
    end,
    
    start_deps(),
    Auth = twitter_util:load_term("../test/fixtures/app_post.fixture"),
    Api = twitter:new(Auth),

    {ok, {Timeline, _}} =
        twitter_search:get(Api, {tweets}, [{q, Query}]),
    display_timeline(Timeline),

    {ok, {NewTimeline, _}} = twitter_rest:prev(Timeline),
    display_timeline(NewTimeline).


start_deps() ->
    lists:foreach(fun(X) -> X:start() end, [crypto, ssl, inets]).


display_timeline(#twitter_timeline{first_id=First, last_id=Last, count=Count}) ->
    io:format("First: ~p, Last: ~p, Count: ~p~n", [First, Last, Count]).
