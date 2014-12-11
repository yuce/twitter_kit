#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves tweets of the given Twitter screen name.
%% Usage: escript statuses_user_timeline.escript [screen_name]

-include("common.hrl").

main(Args) ->
    SName = case Args of
        [] -> "tklx";
        [H|_] -> H
    end,

    start_deps(),
    Api = get_api(),

    {ok, {Timeline, _}} =
        twitter_statuses:get(Api, user_timeline, [{screen_name, SName}]),
    display_timeline(Timeline),

    {ok, {NewTimeline, _}} = twitter:prev(Timeline),
    display_timeline(NewTimeline),

    {ok, {AnotherTimeline, _}} = twitter:next(NewTimeline),
    display_timeline(AnotherTimeline),

    {ok, {YetAnotherTimeline, _}} = twitter:prev(AnotherTimeline),
    display_timeline(YetAnotherTimeline).


display_timeline(#twitter_timeline{first_id=First, last_id=Last, count=Count}) ->
    io:format("First: ~p, Last: ~p, Count: ~p~n", [First, Last, Count]).
