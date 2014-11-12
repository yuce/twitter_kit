#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves the last tweet of the given Twitter screen name.
%% Usage: escript simple_user_timeline [screen_name]

main(Args) ->
    ScreenName = case Args of
        [] -> "tklx";
        [H|_] -> H
    end,
    lists:foreach(fun(X) -> X:start() end, [crypto, ssl, inets]),
    Auth = twitter_util:load_term("../test/fixtures/app_post.fixture"),    
    Api = twitter:new(Auth),
    {ok, Tweets} = twitter_rest:get(Api, "statuses/user_timeline", 
            [{screen_name, ScreenName}, {count, 1}]),
    io:format("~p~n", [Tweets]).
