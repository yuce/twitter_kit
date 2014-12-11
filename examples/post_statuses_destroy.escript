#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves tweets of the authenticated Twitter user.
%% Usage: escript post_statuses_destroy.escript status_id

-include("common.hrl").


main([]) ->
    io:format("Usage: escript post_statuses_destroy.escript status_id~n");

main([StatusId|_]) ->
    start_deps(),
    Api = get_api(),
    {ok, R} = twitter:post(Api, {statuses, destroy, StatusId}, []),
    io:format("~p~n", [R]).

