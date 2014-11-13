#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves tweets of the authenticated Twitter user.
%% Usage: escript post_statuses_destroy.escript status_id

-include("../src/twitter.hrl").

main([]) ->
    io:format("Usage: escript post_statuses_destroy.escript status_id~n");

main([H|_]) ->
    start_deps(),
    Auth = twitter_util:load_term("../test/fixtures/oauth_post.fixture"),
    Api = twitter:new(Auth),
    {ok, R} = twitter_statuses:post(Api, destroy, H, []),
    io:format("~p~n", [R]).


start_deps() ->
    lists:foreach(fun(X) -> X:start() end, [ssl, inets]).

