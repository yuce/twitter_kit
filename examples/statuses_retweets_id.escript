#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves retweets of the given Twitter status ID.
%% Usage: escript statuses_retweets_id.escript [status_ID]

-include("common.hrl").


main(Args) ->
    StatusId = case Args of
        [] -> 537274287523385344;
        [H|_] -> list_to_integer(H)
    end,

    start_deps(),
    Api = get_api(),
    {ok, Items} = twitter:get(Api, {statuses, retweets, StatusId}, [{count, 100}]),
    display(Items).


display(Items) ->
    io:format("Count: ~p~n", [length(Items)]).
