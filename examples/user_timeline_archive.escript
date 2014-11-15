#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves all tweets of the given Twitter screen name.
%% Usage: escript user_timeline_archive [screen_name]

main(Args) ->
    ScreenName = case Args of
        [] -> "tklx";
        [H|_] -> H
    end,
    lists:foreach(fun(X) -> X:start() end, [crypto, ssl, inets]),
    Auth = twitter_util:load_term("../test/fixtures/app_post.fixture"),    
    Api = twitter:new(Auth),
    {ok, {Pointer, Tweets}} = twitter_rest:get(Api, "statuses/user_timeline", 
            [{screen_name, ScreenName}, {count, 200}]),
    AllTweets = get_archive(Pointer, Tweets, []),
    io:format("Fetched ~p tweets.~n", [length(AllTweets)]).


get_archive(_Pointer, [], AllTweets) ->
    AllTweets;

get_archive(Pointer, Tweets, AllTweets) ->
    NewAllTweets = lists:append(Tweets, AllTweets),
    {ok, {NewPointer, NewTweets}} = twitter_rest:get_prev(Pointer),
    get_archive(NewPointer, NewTweets, NewAllTweets).
