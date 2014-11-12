#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves followers of the Twitter use with the given screen name.
%% Usage: escript friends_list.escript [screen_name]

-include("../src/twitter.hrl").


main(Args) ->
    ScreenName = case Args of
        [] -> "cnn";
        [H|_] -> H
    end,
    
    start_deps(),
    Auth = twitter_util:load_term("../test/fixtures/app_post.fixture"),
    Api = twitter:new(Auth),
    {ok, {Cursor, Items}} = 
        twitter_followers:get(Api, list, [{screen_name, ScreenName}, {count, 10}]),
    display(Cursor, Items),
    {ok, {NewCursor, NewItems}} = twitter_rest:next(Cursor),
    display(NewCursor, NewItems).


start_deps() ->
    lists:foreach(fun(X) -> X:start() end, [crypto, ssl, inets]).


display(#twitter_cursor{prev=Prev, next=Next, key=Key}, Items) ->
    io:format("Prev: ~p, Next: ~p, Key: ~p Count: ~p ~n",
                [Prev, Next, Key, length(Items)]).
