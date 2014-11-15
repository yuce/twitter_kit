#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Uploads media to Twitter.
%% Usage: escript post_media_update.escript file1 file2...


main([]) ->
    io:format("Usage: escript post_media_update.escript file1 file2...~n");


main(Args) ->
    start_deps(),
    Auth = twitter_util:load_term("../test/fixtures/oauth_post.fixture"),
    Api = twitter:new(Auth),
    lists:foreach(fun(FileName) -> upload(Api, FileName) end, Args).


upload(Api, FileName) ->
    {ok, Binary} = file:read_file(FileName),
    BaseName = filename:basename(FileName),
    {ok, Data} = twitter_media:post(Api, upload, {media, BaseName, Binary}),
    io:format("Response: ~p~n~n", [Data]).


start_deps() ->
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets).

