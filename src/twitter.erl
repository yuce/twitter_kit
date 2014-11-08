%%%-------------------------------------------------------------------
%%% @author Yuce
%%% @copyright (C) 2013, <Yuce Tekol>
%%% @doc Twitter API for erlang
%%%
%%% @end
%%% Created : 06. Ara 2013 03:15
%%%-------------------------------------------------------------------
-module(twitter).
-author("Yuce Tekol").

%% API
-export([new/1, get/3]).
-export([make_get/1]).
-export([make_app/1]).

-include("oauth.hrl").
-include("twitter.hrl").
-include("util.hrl").

-spec new(#oauth{}) -> #twitter{}.

%% @doc Create new twitter record.
new(Auth) ->
  #twitter{auth = Auth}.

%-spec get(#twitter{}, path(), [param()]) -> reply().
%-type path() :: atom() | string().
%-type reply() :: {ok, jsx:json_term()} | {error, {integer(), binary()}}
%        | {error, term()}.

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


%-spec make_url(#twitter{}, path(), [param()]) -> string().
%-type param()          :: {param_key(), param_value()}.
%-type param_key()      :: atom().
%-type param_value()    :: string().

make_url(#twitter{domain=Domain, api_version=ApiVersion,
            secure=Secure, format=Format}, ApiPath, QryStr) ->
    Scheme = ?select(Secure, "https", "http"),
    Path = lists:concat([ApiVersion, "/", ApiPath, ".", Format]),
    twikit_util:make_url({Scheme, Domain, Path, QryStr}).

make_get(Twitter) ->
    fun(Path, Args) ->
        get(Twitter, Path, Args)
    end.

-spec make_app(#twitter{}) -> #twitter{}.

make_app(Twitter) ->
    #twitter{auth=Oauth} = Twitter,
    AppCreds = oauth:make_app_creds(Oauth),
    Headers = [{"authorization", lists:append("Basic ", AppCreds)}],
    ContentType = "application/x-www-form-urlencoded;charset=UTF-8",
    RequestBody = "grant_type=client_credentials",
    Url = "https://api.twitter.com/oauth2/token",
    Request = {Url, Headers, ContentType, RequestBody},
    case httpc:request(post, Request, [], [{body_format, binary}]) of
        {ok, Response} ->
            {{_, Status, _}, _, Body} = Response,
            if Status == 200 ->
                % TODO: check token type is "bearer"
                {_, Token} = lists:keyfind(<<"access_token">>, 1, jsx:decode(Body)),
                Twitter#twitter{auth=Oauth#oauth{app_token=binary_to_list(Token)}};
            true ->
                {error, Status, Body}
            end;
        Reply ->
            Reply
    end.




