-module(twitter_rest).
-author("Yuce Tekol").

-export([get/3, get_next/1, get_prev/1]).

-include("twitter.hrl").
-include("util.hrl").
-include("def.hrl").


-spec get(#twitter{}, path(), query_args()) -> get_result().
-type get_result() :: {ok, {#twitter_pointer{}, list()}}.

get(#twitter{auth=#oauth{token=Token} = Auth,
             json_decode=JsonDecode} = Twitter, Path, Args)
        when Token =/= "" ->
    BaseUrl = make_url(Twitter, Path, ""),
    Request = twitter_auth:make_signed_request(Auth, get, BaseUrl, Args), 
    {ok, Body} = request(Request),
    Tweets = JsonDecode(Body),
    {ok, {make_pointer(Twitter, Path, Args, Tweets), Tweets}};

get(#twitter{auth=#oauth{app_token=BT} = Auth,
             json_decode=JsonDecode} = Twitter, Path, Args)
        when BT =/= "" ->
    Url = make_url(Twitter, Path, twitter_util:encode_qry(Args)),
    Request = twitter_auth:make_app_request(Auth, Url),
    {ok, Body} = request(Request),
    Tweets = JsonDecode(Body),
    {ok, {make_pointer(Twitter, Path, Args, Tweets), Tweets}}.


-spec get_next(#twitter_pointer{}) -> get_result().

get_next(#twitter_pointer{api=Api,
                          path=Path,
                          args=Args,
                          last_id=Last}) ->
    NewArgs = ?select(Last > 0,
                lists:keystore(since_id, 1, Args, {since_id, Last}),
                Args),
    get(Api, Path, NewArgs).


-spec get_prev(#twitter_pointer{}) -> get_result().

get_prev(#twitter_pointer{api=Api,
                          path=Path,
                          args=Args,
                          first_id=First}) ->
    NewArgs = ?select(First > 0,
                lists:keystore(max_id, 1, Args, {max_id, First - 1}),
                Args),
    get(Api, Path, NewArgs).


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


-spec make_pointer(#twitter{}, path(), query_args(), list())
        -> #twitter_pointer{}.

make_pointer(Api, Path, Args, Tweets) ->
    {Count, First, Last} = get_count_first_last_tweet_id(Tweets),
    #twitter_pointer{
        api = Api,
        path = Path,
        args = Args,
        first_id = First,
        last_id = Last,
        count = Count}.


-spec get_tweet_id(list()) -> tweet_id().

get_tweet_id(Tweet) ->
    {_, Id} = lists:keyfind(<<"id">>, 1, Tweet),
    Id.


-spec get_count_first_last_tweet_id(list())
    -> {tweet_id(), tweet_id(), tweet_id()}.

get_count_first_last_tweet_id([]) ->
    {0, 0, 0};

get_count_first_last_tweet_id([H|T]) ->
    F = fun(X, {Count, _Last}) -> {Count + 1, X} end,
    {Count, First} = lists:foldl(F, {1, H}, T),
    {Count, get_tweet_id(First), get_tweet_id(H)}.


%% Internal Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_tweet_id_test() ->
    Tweet = twitter_util:load_term("../test/fixtures/tweet.fixture"),
    Id = get_tweet_id(Tweet),
    ?assertEqual(Id, 531969892808159232).

-endif.
