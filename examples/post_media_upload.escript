#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Uploads media to Twitter.
%% Usage: escript post_media_update.escript file1 file2...

-include("common.hrl").


main([]) ->
    io:format("Usage: escript post_media_update.escript file1 file2...~n");


main(Args) ->
    start_deps(),
    Api = get_api(),
    lists:foreach(fun(FileName) -> upload(Api, FileName) end, Args).


upload(Api, FileName) ->
    {ok, Binary} = file:read_file(FileName),
    BaseName = filename:basename(FileName),
    {ok, Data} = twitter:post(Api, {media, upload}, {media, BaseName, Binary}),
    io:format("Response: ~p~n~n", [Data]).
