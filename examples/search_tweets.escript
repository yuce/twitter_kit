#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Search tweets.
%% Usage: escript search_tweets.escript [query]

-include("common.hrl").


main(Args) ->
    Query = case Args of
        [] -> "erlang";
        [H|_] -> H
    end,

    start_deps(),
    Api = get_api(),

    {ok, {Timeline, _}} =
        twitter:get(Api, {search, tweets}, [{q, Query}]),
    display_timeline(Timeline),

    {ok, {NewTimeline, _}} = twitter:prev(Timeline),
    display_timeline(NewTimeline).


display_timeline(#twitter_timeline{first_id=First, last_id=Last, count=Count}) ->
    io:format("First: ~p, Last: ~p, Count: ~p~n", [First, Last, Count]).
