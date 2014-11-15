#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Send a tweet with a photo attached.
%% Usage: escript send_tweet_with_media.escript message file_name


main([Message, FileName]) ->
    start_deps(),
    Auth = twitter_util:load_term("../test/fixtures/oauth_post.fixture"),
    Api = twitter:new(Auth),
    % First upload the image
    MediaId = upload(Api, FileName),
    % Post the tweet
    {ok, Item} = twitter_statuses:post(Api, update,
                                       [{status, Message},
                                        {media_ids, MediaId}]),
    io:format("~p~n", [Item]);

main(_) ->
    io:format("Usage: escript send_tweet_with_media.escript message file_name~n").



upload(Api, FileName) ->
    {ok, Binary} = file:read_file(FileName),
    BaseName = filename:basename(FileName),
    {ok, Data} = twitter_media:post(Api, upload, {media, BaseName, Binary}),
    {_, MediaId} = lists:keyfind(<<"media_id">>, 1, Data),
    MediaId.


start_deps() ->
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets).

