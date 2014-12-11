#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../ebin ../deps/jsx/ebin

%% Retrieves tweets of the authenticated Twitter user.
%% Usage: escript post_statuses_update.escript [message]

-include("common.hrl").

main([]) ->
    random:seed(os:timestamp()),
    RangeList = [{$a, $z}, {$A, $Z}, $., {32, 32}, {$0, $9}],
    SimpleAlphabet = twitter_util:alphabet_of(RangeList),
    Message = twitter_util:random_string(SimpleAlphabet, 100),
    main([Message]);

main([Message]) ->
    start_deps(),
    Api = get_api(),
    {ok, Item} = twitter:post(Api, {statuses, update},
                              [{status, Message},
                               {trim_user, "true"}]),
    io:format("~p~n", [Item]);

main(_) ->
    io:format("Usage: Usage: escript post_statuses_update.escript [message]~n").

