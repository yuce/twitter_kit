#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves tweets of the authenticated Twitter user.
%% Usage: escript statuses_home_timeline.escript

-include("common.hrl").


main(_Args) ->
    start_deps(),
    Api = get_api(),
    {ok, {Timeline, _Items}} = twitter:get(Api, {statuses, home_timeline}, []),
    iter(Api, Timeline, 1).


display_timeline(#twitter_timeline{first_id=First,
                                   last_id=Last}) ->
    io:format("First: ~p, Last: ~p~n", [First, Last]).


iter(Api, Timeline, N) ->
    display_timeline(Timeline),
    if  N > 1 ->
            case twitter:prev(Timeline) of
                {ok, {NewTimeline, _}} ->
                    iter(Api, NewTimeline, N - 1);
                {stop, _} ->
                    io:format("No more tweets.~n") end;
        true ->
            io:format("End of run.~n") end.


