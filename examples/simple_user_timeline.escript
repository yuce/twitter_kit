#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves the last tweet of the given Twitter screen name.
%% Usage: escript simple_user_timeline [screen_name]

-include("common.hrl").

main(Args) ->
    ScreenName = case Args of
        [] -> "twitter";
        [H|_] -> H end,
    start_deps(),
    Api = get_api(),
    {ok, {_Timeline, Tweets}} = twitter:get(Api, {statuses, user_timeline},
                                            [{screen_name, ScreenName},
                                             {count, 1}]),
    io:format("~p~n", [Tweets]).
