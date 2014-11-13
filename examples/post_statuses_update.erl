#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves tweets of the authenticated Twitter user.
%% Usage: escript statuses_home_timeline.escript

-include("../src/twitter.hrl").

main(Args) ->
    Message = case Args of
        [] -> "Ağaç dalında İki köpek, bir kuş...";
        [H|_] -> H
    end,

    start_deps(),
    Auth = twitter_util:load_term("../test/fixtures/oauth_post.fixture"),
    Api = twitter:new(Auth),
    {ok, Item} = twitter_statuses:post(Api, update, 
                                       [{status, Message}, 
                                        {trim_user, "true"}]),
    io:format("~p~n", [Item]).


start_deps() ->
    lists:foreach(fun(X) -> X:start() end, [crypto, ssl, inets]).

