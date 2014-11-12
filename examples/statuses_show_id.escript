#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves retweets of the given Twitter status ID.
%% Usage: escript statuses_show_id.escript [status_ID]

-include("../src/twitter.hrl").


main(Args) ->
    StatusId = case Args of
        [] -> 532100719382646785;
        [H|_] -> list_to_integer(H)
    end,
    
    start_deps(),
    Auth = twitter_util:load_term("../test/fixtures/app_post.fixture"),
    GetStatuses = twitter_statuses:get(twitter:new(Auth)),
    {ok, Tweet} = GetStatuses({show, StatusId}, []),
    display(Tweet).


start_deps() ->
    lists:foreach(fun(X) -> X:start() end, [crypto, ssl, inets]).


display(Tweet) ->
    io:format("~p~n", [Tweet]).
