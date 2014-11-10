-module(twitter_rest).
-author("Yuce Tekol").

-export([get/3]).
-export([make_get/1]).

-include("twitter.hrl").
-include("oauth.hrl").
-include("util.hrl").
-include("def.hrl").


-spec get(#twitter{}, path(), query_args()) -> get_result().
-type get_result() :: {ok, term()} | {error, term()}.

%% @doc Access Twitter endpoint with Path and Arguments.
get(#twitter{auth=#oauth{token=Token}=Auth}=Twitter, Path, Args)
        when Token =/= "" ->
    BaseUrl = make_url(Twitter, Path, ""),
    Request = twitter_auth:make_signed_request(Auth, get, BaseUrl, Args), 
    request(Request);

get(#twitter{auth=#oauth{app_token=BT}=Auth}=Twitter, Path, Args)
        when BT =/= "" ->
    Url = make_url(Twitter, Path, twitter_util:encode_qry(Args)),
    Request = twitter_auth:make_app_request(Auth, Url),
    request(Request).

-spec request(request()) -> get_result().

request(Request) ->
    case httpc:request(get, Request, [], [{body_format, binary}]) of
        {ok, Response} ->
            {{_, Status, _}, _, Body} = Response,
            ?select(Status == 200, {ok, jsx:decode(Body)},
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

-spec make_get(#twitter{}) -> fun((path(), query_args()) -> get_result()).    

make_get(Twitter) ->
    fun(Path, Args) ->
        get(Twitter, Path, Args)
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_with_app_token_test() ->
    Auth = twitter_util:load_term("../test/fixtures/app_post.fixture"),
    Tw = twitter:new(Auth),
    {ok, _Tweets} =
        get(Tw, "statuses/user_timeline", [{screen_name, "tklx"}]).

-endif.
