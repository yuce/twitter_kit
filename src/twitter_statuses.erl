-module(twitter_statuses).
-author("Yuce Tekol").

-export([get/1]).

-include("twitter.hrl").
-include("util.hrl").


get(Api) ->
    fun
        ({retweeters, ids}, Args) ->
            Path = "statuses/retweeters/ids",
            {ok, Items} = twitter_rest:get(Api, Path, Args),
            {ok, make_cursor(Api, Path, Args, Items, "ids")};

        ({retweets, Id}, Args) ->
            twitter_rest:get(Api, lists:concat(["statuses/retweets/", Id]), Args);

        ({show, Id}, Args) ->
            twitter_rest:get(Api, lists:concat(["statuses/show/", Id]), Args);

        (oembed, Args) ->
            twitter_rest:get(Api, "statuses/oembed", Args);

        (lookup, Args) ->
            twitter_rest:get(Api, "statuses/lookup", Args);

        (#twitter_cursor{api=A, path=Path, args=OldArgs, prev=Prev, key=Key}, prev)
                when Prev > 0 ->
            A = Api,  % Do not allow rogue cursors. For now?...
            Args = lists:keystore(cursor, 1, OldArgs, {cursor, Prev}),
            ret_cursor(Api, Path, Args, Key);

        (#twitter_cursor{} = Cursor, prev) ->
            {stop, Cursor};

        (#twitter_cursor{api=A, path=Path, args=OldArgs, next=Next, key=Key}, next)
                when Next > 0 ->
            A = Api,  % Do not allow rogue cursors. For now?...
            Args = lists:keystore(cursor, 1, OldArgs, {cursor, Next}),
            ret_cursor(Api, Path, Args, Key);

        (#twitter_cursor{} = Cursor, next) ->
            {stop, Cursor};

        (#twitter_timeline{api=A, path=Path, args=OldArgs, first_id=First}, prev)
                when First > 0 ->
            A = Api,  % Do not allow rogue cursors. For now?...
            ModArgs = lists:keydelete(since_id, 1, OldArgs),
            Args = lists:keystore(max_id, 1, ModArgs, {max_id, First - 1}),
            ret_timeline(Api, Path, Args);

        (#twitter_timeline{} = Timeline, prev) ->
            {stop, Timeline};

        (#twitter_timeline{api=A, path=Path, args=OldArgs, last_id=Last}, next)
                when Last > 0 ->
            A = Api,  % Do not allow rogue cursors. For now?...
            ModArgs = lists:keydelete(max_id, 1, OldArgs),
            Args = lists:keystore(since_id, 1, ModArgs, {since_id, Last}),
            ret_timeline(Api, Path, Args);

        (#twitter_timeline{} = Timeline, next) ->
            {stop, Timeline};

        (Path, Args) ->
            NewPath = lists:concat(["statuses/", Path]),
            {ok, Items} = twitter_rest:get(Api, NewPath, Args),
            {ok, {make_timeline(Api, NewPath, Args, Items), Items}}
    end.


ret_cursor(Api, Path, Args, Key) ->
    {ok, Items} = twitter_rest:get(Api, Path, Args),
    {ok, make_cursor(Api, Path, Args, Items, Key)}.


ret_timeline(Api, Path, Args) ->
    {ok, Items} = twitter_rest:get(Api, Path, Args),
    Timeline = make_timeline(Api, Path, Args, Items),
    #twitter_timeline{count=Count} = Timeline,
    ?select(Count == 0, {stop, Timeline}, {ok, {Timeline, Items}}).


make_cursor(Api, Path, Args, Data, CollectionKey) ->
    {Items, Prev, Next} = get_count_prev_next(Data, CollectionKey),
    {#twitter_cursor{api = Api,
                    path = Path,
                    args = Args,
                    key = CollectionKey,
                    prev = Prev,
                    next = Next}, Items}.


make_timeline(Api, Path, Args, Tweets) ->
    {Count, First, Last} = get_count_first_last_tweet_id(Tweets),
    #twitter_timeline{
        api = Api,
        path = Path,
        args = Args,
        first_id = First,
        last_id = Last,
        count = Count}.


get_count_prev_next(Data, Key) ->
    {_, Prev} = lists:keyfind(<<"previous_cursor">>, 1, Data),
    {_, Next} = lists:keyfind(<<"next_cursor">>, 1, Data),
    {_, Items} = lists:keyfind(list_to_binary(Key), 1, Data),
    {Items, Prev, Next}.


get_tweet_id(Tweet) ->
    {_, Id} = lists:keyfind(<<"id">>, 1, Tweet),
    Id.


get_count_first_last_tweet_id([]) ->
    {0, 0, 0};

get_count_first_last_tweet_id([H|T]) ->
    F = fun(X, {Count, _Last}) -> {Count + 1, X} end,
    {Count, First} = lists:foldl(F, {1, H}, T),
    {Count, get_tweet_id(First), get_tweet_id(H)}.

