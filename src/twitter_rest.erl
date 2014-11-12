-module(twitter_rest).
-author("Yuce Tekol").

-export([get/3]).

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
