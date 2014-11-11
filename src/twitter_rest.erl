-module(twitter_rest).
-author("Yuce Tekol").

-export([get/3]).

-include("twitter.hrl").
-include("oauth.hrl").
-include("util.hrl").
-include("def.hrl").


-spec get(#twitter{}, path(), query_args()) -> get_result().
-type get_result() :: {ok, #twitter_chunk{}} | {error, term()}.

%% @doc Access Twitter endpoint with Path and Arguments.
get(#twitter{auth=#oauth{token=Token}=Auth}=Twitter, Path, Args)
        when Token =/= "" ->
    BaseUrl = make_url(Twitter, Path, ""),
    Request = twitter_auth:make_signed_request(Auth, get, BaseUrl, Args), 
    request(Twitter, Request);

get(#twitter{auth=#oauth{app_token=BT}=Auth}=Twitter, Path, Args)
        when BT =/= "" ->
    Url = make_url(Twitter, Path, twitter_util:encode_qry(Args)),
    Request = twitter_auth:make_app_request(Auth, Url),
    request(Twitter, Request).


-spec request(#twitter{}, request()) -> get_result().

request(Twitter, Request) ->
    case httpc:request(get, Request, [], [{body_format, binary}]) of
        {ok, Response} ->
            {{_, Status, _}, _, Body} = Response,
            ?select(Status == 200, {ok, chunkize(Twitter, Body)},
                {error, {Status, Body}});
        {error, _Reason}=Reply ->
            Reply
    end.


-spec make_url(#twitter{}, path(), query_string()) -> url().

make_url(#twitter{domain=Domain, api_version=ApiVersion,
            secure=Secure, format=Format}, ApiPath, QryStr) ->
    Scheme = ?select(Secure, "https", "http"),
    Path = lists:concat([ApiVersion, "/", ApiPath, ".", Format]),
    twitter_util:make_url({Scheme, Domain, Path, QryStr}).


-spec chunkize(#twitter{}, binary()) -> #twitter_chunk{}.

chunkize(Twitter, Binary) ->
    Tweets = jsx:decode(Binary),
    {Count, First, Last} = get_count_first_last_tweet_id(Tweets),
    #twitter_chunk{
        api=Twitter,
        tweets=Tweets,
        first_id=First,
        last_id=Last,
        count=Count
    }.


-spec get_tweet_id(list()) -> tweet_id().

get_tweet_id(Tweet) ->
    {_, Id} = lists:keyfind(<<"id">>, 1, Tweet),
    Id.


-spec get_count_first_last_tweet_id(list())
    -> {tweet_id(), tweet_id(), tweet_id()}.

get_count_first_last_tweet_id([]) ->
    {0, 0, 0};

get_count_first_last_tweet_id([H|T]) ->
    F = fun(X, {Count, _Last}) -> {Count + 1, get_tweet_id(X)} end,
    {Count, First} = lists:foldl(F, {1, H}, T),
    {Count, First, get_tweet_id(H)}.


%% Internal Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_tweet_id_test() ->
    Tweet = twitter_util:load_term("../test/fixtures/tweet.fixture"),
    Id = get_tweet_id(Tweet),
    ?assertEqual(Id, 531969892808159232).

-endif.
