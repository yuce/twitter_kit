#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves tweets of the given Twitter screen name.
%% Usage: escript statuses_user_timeline.escript [screen_name]

-include("../src/twitter.hrl").

main(Args) ->
    SName = case Args of
        [] -> "tklx";
        [H|_] -> H
    end,
    
    start_deps(),
    Auth = twitter_util:load_term("../test/fixtures/app_post.fixture"),
    Api = twitter:new(Auth),

    {ok, {Timeline, _}} =
        twitter_statuses:get(Api, {user_timeline}, [{screen_name, SName}]),
    display_timeline(Timeline),

    {ok, {NewTimeline, _}} = twitter_rest:prev(Timeline),
    display_timeline(NewTimeline),

    {ok, {AnotherTimeline, _}} = twitter_rest:next(NewTimeline),
    display_timeline(AnotherTimeline),

    {ok, {YetAnotherTimeline, _}} = twitter_rest:prev(AnotherTimeline),
    display_timeline(YetAnotherTimeline).


start_deps() ->
    lists:foreach(fun(X) -> X:start() end, [crypto, ssl, inets]).


display_timeline(#twitter_timeline{first_id=First, last_id=Last, count=Count}) ->
    io:format("First: ~p, Last: ~p, Count: ~p~n", [First, Last, Count]).
