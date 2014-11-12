#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves retweets of the given Twitter status ID.
%% Usage: escript statuses_retweets_id.escript [status_ID]

-include("../src/twitter.hrl").


main(Args) ->
    StatusId = case Args of
        [] -> 532100719382646785;
        [H|_] -> list_to_integer(H)
    end,
    
    start_deps(),
    Auth = twitter_util:load_term("../test/fixtures/app_post.fixture"),
    GetStatuses = twitter_statuses:get(twitter:new(Auth)),
    {ok, Items} = GetStatuses({retweets, StatusId}, [{count, 100}]),
    display(Items).


start_deps() ->
    lists:foreach(fun(X) -> X:start() end, [crypto, ssl, inets]).


display(Items) ->
    io:format("Count: ~p~n", [length(Items)]).
