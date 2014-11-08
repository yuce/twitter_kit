
-module(twitter).
-author("Yuce Tekol").

-export([new/1, get/3]).
-export([make_get/1]).
-export([auth_app/1]).

-include("oauth.hrl").
-include("twitter.hrl").
-include("util.hrl").

-spec new(#oauth{}) -> #twitter{}.

%% @doc Create new twitter record.
new(Auth) ->
  #twitter{auth = Auth}.

%% @doc Access Twitter endpoint with Path and Arguments.
get(#twitter{auth=#oauth{token=Token}=Auth}=Twitter, Path, Args)
        when Token =/= "" ->
    BaseUrl = make_url(Twitter, Path, ""),
    Request = oauth:make_signed_request(Auth, "GET", BaseUrl, Args), 
    request(Request);

get(#twitter{auth=#oauth{app_token=BT}=Auth}=Twitter, Path, Args)
        when BT =/= "" ->
    Url = make_url(Twitter, Path, twikit_util:encode_qry(Args)),
    Request = oauth:make_app_request(Auth, Url),
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

-spec auth_app(#twitter{}) -> {ok | error, #twitter{}}.

auth_app(Twitter) ->
    #twitter{auth=Auth} = Twitter,
    AppCreds = oauth:make_app_creds(Auth),
    Headers = [{"authorization", lists:append("Basic ", AppCreds)}],
    ContentType = "application/x-www-form-urlencoded;charset=UTF-8",
    RequestBody = "grant_type=client_credentials",
    Url = "https://api.twitter.com/oauth2/token",
    Request = {Url, Headers, ContentType, RequestBody},
    case httpc:request(post, Request, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            % TODO: check token type is "bearer"
            {_, Token} = lists:keyfind(<<"access_token">>, 1,
                jsx:decode(Body)),
            NewAuth = Auth#oauth{app_token=binary_to_list(Token)},
            {ok, Twitter#twitter{auth=NewAuth}};
        {ok, {{_, ErrorStatus, _}, _, Body}} ->
            {error, ErrorStatus, Body};
        {error, Response} ->
            {error, Response}
    end.

