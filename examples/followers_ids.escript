#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves followers of the Twitter use with the given screen name.
%% Usage: escript friends_ids.escript [screen_name]

-include("common.hrl").


main(Args) ->
    ScreenName = case Args of
        [] -> "cnn";
        [H|_] -> H
    end,

    start_deps(),
    Api = get_api(),
    {ok, {Cursor, Items}} =
        twitter:get(Api, {followers, ids}, [{screen_name, ScreenName}, {count, 10}]),
    display(Cursor, Items),
    {ok, {NewCursor, NewItems}} = twitter_rest:next(Cursor),
    display(NewCursor, NewItems).


display(#twitter_cursor{prev=Prev, next=Next, key=Key}, Items) ->
    io:format("Prev: ~p, Next: ~p, Key: ~p Count: ~p ~n",
                [Prev, Next, Key, length(Items)]).
