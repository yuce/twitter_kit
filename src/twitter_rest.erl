-module(twitter_rest).
-author("Yuce Tekol").

-export([get/3, prev/1, next/1]).
-export([make_cursor/5, make_timeline/4]).

-include("twitter.hrl").
-include("util.hrl").
-include("def.hrl").


-spec get(#twitter{}, path(), query_args()) -> get_result().
-type get_result() :: {ok, term()}.

get(#twitter{auth=#oauth{token=Token} = Auth,
             json_decode=JsonDecode} = Twitter, Path, Args)
        when Token =/= "" ->
    BaseUrl = make_url(Twitter, Path, ""),
    Request = twitter_auth:make_signed_request(Auth, get, BaseUrl, Args), 
    {ok, Body} = request(Request),
    {ok, JsonDecode(Body)};

get(#twitter{auth=#oauth{app_token=BT} = Auth,
             json_decode=JsonDecode} = Twitter, Path, Args)
        when BT =/= "" ->
    Url = make_url(Twitter, Path, twitter_util:encode_qry(Args)),
    Request = twitter_auth:make_app_request(Auth, Url),
    {ok, Body} = request(Request),
    {ok, JsonDecode(Body)}.


prev(#twitter_cursor{api=Api, path=Path, args=OldArgs, prev=Prev, key=Key})
        when Prev > 0 ->
    Args = lists:keystore(cursor, 1, OldArgs, {cursor, Prev}),
    ret_cursor(Api, Path, Args, Key);

prev(#twitter_cursor{} = Cursor) ->
    {stop, Cursor};

prev(#twitter_timeline{api=Api, path=Path, args=OldArgs, first_id=First})
        when First > 0 ->
    ModArgs = lists:keydelete(since_id, 1, OldArgs),
    Args = lists:keystore(max_id, 1, ModArgs, {max_id, First - 1}),
    ret_timeline(Api, Path, Args);

prev(#twitter_timeline{} = Timeline) ->
    {stop, Timeline}.


next(#twitter_cursor{api=Api, path=Path, args=OldArgs, next=Next, key=Key})
        when Next > 0 ->
    Args = lists:keystore(cursor, 1, OldArgs, {cursor, Next}),
    ret_cursor(Api, Path, Args, Key);

next(#twitter_cursor{} = Cursor) ->
    {stop, Cursor};

next(#twitter_timeline{api=Api, path=Path, args=OldArgs, last_id=Last})
        when Last > 0 ->
    ModArgs = lists:keydelete(max_id, 1, OldArgs),
    Args = lists:keystore(since_id, 1, ModArgs, {since_id, Last}),
    ret_timeline(Api, Path, Args);

next(#twitter_timeline{} = Timeline) ->
    {stop, Timeline}.


ret_cursor(Api, Path, Args, Key) ->
    {ok, Items} = get(Api, Path, Args),
    {ok, make_cursor(Api, Path, Args, Items, Key)}.


ret_timeline(Api, Path, Args) ->
    {ok, Items} = get(Api, Path, Args),
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


-spec request(request()) -> request_result().
-type request_result() :: {ok, list()} | {error, term()}.

request(Request) ->
    case httpc:request(get, Request, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, Body};
        {ok, {{_, Status, _}, _, Body}} ->
            {error, {Status, Body}};
        {error, _Reason} = Reply ->
            Reply
    end.


-spec make_url(#twitter{}, path(), query_string()) -> url().

make_url(#twitter{domain = Domain,
                  api_version = ApiVersion,
                  secure = Secure,
                  format = Format}, ApiPath, QryStr) ->
    Scheme = ?select(Secure, "https", "http"),
    Path = lists:concat([ApiVersion, "/", ApiPath, ".", Format]),
    twitter_util:make_url({Scheme, Domain, Path, QryStr}).
