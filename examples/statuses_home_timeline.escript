#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves tweets of the authenticated Twitter user.
%% Usage: escript statuses_home_timeline.escript

-include("../src/twitter.hrl").

%% FIX!!!!

main(_Args) ->
    start_deps(),
    Auth = twitter_util:load_term("../test/fixtures/oauth_post.fixture"),
    Api = twitter:new(Auth),
    iter(Api, 1).

start_deps() ->
    lists:foreach(fun(X) -> X:start() end, [crypto, ssl, inets]).


display_timeline(#twitter_timeline{first_id=First, last_id=Last, count=Count}) ->
    io:format("First: ~p, Last: ~p, Count: ~p~n", [First, Last, Count]).


iter(_Api, 0) ->
    io:format("End of run.~n");

iter(Api, N) when N > 0 ->
    case twitter_statuses:get(Api, home_timeline, []) of
        {ok, {Timeline, _}} ->
            display_timeline(Timeline),
            iter(Api, N - 1);
        {stop, _} ->
            io:format("No more tweets.~n") end.

