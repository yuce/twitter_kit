#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Send a tweet with a photo attached.
%% Usage: escript send_tweet_with_media.escript message file_name
%% Note that, Twitter supports attaching upto 4 images to statuses (2014-11)

-include("common.hrl").


main([Message|FileNames]) ->
    start_deps(),
    Api = get_api(),
    % First upload the image
    F = fun(FN) ->
        Id = upload(Api, FN),
        integer_to_list(Id) end,

    MediaIds = string:join(lists:map(F, FileNames), ","),
    % Post the tweet
    {ok, Item} = twitter:post(Api, {statuses, update},
                                       get_args(Message, MediaIds)),
    io:format("~p~n", [Item]);

main(_) ->
    io:format("Usage: escript send_tweet_with_media.escript "
              "message file1 file2...~n").


get_args(Message, []) ->
    [{status, Message}];

get_args(Message, MediaIds) ->
    [{status, Message}, {media_ids, MediaIds}].


upload(Api, FileName) ->
    {ok, Binary} = file:read_file(FileName),
    BaseName = filename:basename(FileName),
    {ok, Data} = twitter:post(Api, {media, upload}, {media, BaseName, Binary}),
    {_, MediaId} = lists:keyfind(<<"media_id">>, 1, Data),
    MediaId.
