
-module(twitter).
-author("Yuce Tekol").

-export([new/1, get/3]).
-export([make_get/1]).

-include("oauth.hrl").
-include("twitter.hrl").
-include("util.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec new(#oauth{}) -> #twitter{}.

%% @doc Create new twitter record.
new(Auth) ->
  #twitter{auth = Auth}.

%% @doc Access Twitter endpoint with Path and Arguments.
get(#twitter{auth=#oauth{token=Token}=Auth}=Twitter, Path, Args)
        when Token =/= "" ->
    BaseUrl = make_url(Twitter, Path, ""),
    Request = twikit_auth:make_signed_request(Auth, "GET", BaseUrl, Args), 
    request(Request);

get(#twitter{auth=#oauth{app_token=BT}=Auth}=Twitter, Path, Args)
        when BT =/= "" ->
    Url = make_url(Twitter, Path, twikit_util:encode_qry(Args)),
    Request = twikit_auth:make_app_request(Auth, Url),
    request(Request).

request(Request) ->
    case httpc:request(get, Request, [], [{body_format, binary}]) of
        {ok, Response} ->
            {{_, Status, _}, _, Body} = Response,
            ?select(Status == 200, {ok, jsx:decode(Body)},
                {error, {Status, Body}});
        {error, _Reason}=Reply ->
            Reply
    end.

make_url(#twitter{domain=Domain, api_version=ApiVersion,
            secure=Secure, format=Format}, ApiPath, QryStr) ->
    Scheme = ?select(Secure, "https", "http"),
    Path = lists:concat([ApiVersion, "/", ApiPath, ".", Format]),
    twikit_util:make_url({Scheme, Domain, Path, QryStr}).

make_get(Twitter) ->
    fun(Path, Args) ->
        get(Twitter, Path, Args)
    end.

-ifdef(TEST).

get_with_app_token_test() ->
    Auth = twikit_util:load_term("../test/fixtures/app_post.fixture"),
    Tw = new(Auth),
    {ok, _Tweets} =
        get(Tw, "statuses/user_timeline", [{screen_name, "tklx"}]).

-endif.
