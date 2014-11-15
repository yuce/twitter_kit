#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves retweeters of the given Twitter status ID.
%% Usage: escript statuses_retweeters_ids.escript [status_ID]
%% NOTE: statuses/retweets/ids returns max 100 user ids,
%% and the cursor is always stopped.

-include("../src/twitter.hrl").


main(Args) ->
    StatusId = case Args of
        [] -> 532100719382646785;
        [H|_] -> list_to_integer(H)
    end,
    
    start_deps(),
    Auth = twitter_util:load_term("../test/fixtures/app_post.fixture"),
    Api = twitter:new(Auth),
    {ok, {Cursor, Items}} = 
        twitter_statuses:get(Api, retweeters, ids, [{id, StatusId}]),
    display(Cursor, Items),
    {stop, _} = twitter_rest:prev(Cursor).


start_deps() ->
    lists:foreach(fun(X) -> X:start() end, [crypto, ssl, inets]).


display(#twitter_cursor{prev=Prev, next=Next, key=Key}, Items) ->
    io:format("Prev: ~p, Next: ~p, Key: ~p Count: ~p ~n",
                [Prev, Next, Key, length(Items)]).
