#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves all tweets of the given Twitter screen name.
%% Usage: escript user_timeline_archive [screen_name]

-include("common.hrl").

main(Args) ->
    ScreenName = case Args of
        [] -> "tklx";
        [H|_] -> H
    end,
    start_deps(),
    Api = get_api(),
    {ok, {Pointer, Tweets}} = twitter:get(Api, {statuses, user_timeline},
                                          [{screen_name, ScreenName},
                                           {count, 200}]),
    AllTweets = get_archive(Pointer, Tweets, []),
    io:format("Fetched ~p tweets.~n", [length(AllTweets)]).


get_archive(_Pointer, [], AllTweets) ->
    AllTweets;

get_archive(Pointer, Tweets, AllTweets) ->
    NewAllTweets = lists:append(Tweets, AllTweets),
    case twitter:prev(Pointer) of
        {ok, {NewPointer, NewTweets}} ->
            get_archive(NewPointer, NewTweets, NewAllTweets);
        {stop, _} ->
            NewAllTweets end.
