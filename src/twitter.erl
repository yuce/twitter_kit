%%%-------------------------------------------------------------------
%%% @author Yuce
%%% @copyright (C) 2013, <Yuce Tekol>
%%% @doc Twitter API for erlang
%%%
%%% @end
%%% Created : 06. Ara 2013 03:15
%%%-------------------------------------------------------------------
-module(twitter).
-author("Yuce").

%% API
-export([new/1, get/3]).
-export([make_get/1]).
-export([make_bearer/1]).

-include("oauth.hrl").
-include("twitter.hrl").
-include("util.hrl").

-spec new(#oauth{}) -> #twitter{}.

%% @doc Create new twitter record.
new(Auth) ->
  #twitter{auth = Auth}.

-spec get(#twitter{}, path(), [param()]) -> reply().
-type path() :: atom() | string().
-type reply() :: {ok, jsx:json_term()} | {error, {integer(), binary()}}
        | {error, term()}.

%% @doc Access Twitter endpoint with Path and Arguments.
get(Twitter, Path, Args) ->
    Uri = make_uri(Twitter, Path, Args),
    case httpc:request(get, {Uri, []}, [], [{body_format, binary}]) of
        {ok, Response} ->
            {{_, Status, _}, _, Body} = Response,
            ?select(Status == 200, {ok, jsx:decode(Body)},
                {error, {Status, Body}});
        {error, _Reason}=Reply ->
            Reply
    end.

-spec make_uri(#twitter{}, path(), [param()]) -> string().
-type param()          :: {param_key(), param_value()}.
-type param_key()      :: atom().
-type param_value()    :: string().

make_uri(#twitter{auth=Auth, domain=Domain, api_version=ApiVersion,
            secure=Secure, format=Format}, Path, Args) ->
    Scheme = ?select(Secure, "https", "http"),
    BaseUrl = lists:concat([Scheme, "://", Domain, "/", 
        ApiVersion, "/", Path, ".", Format]),
    EncodedParams = oauth:encode_params(Auth, BaseUrl, "GET", Args),
    string:join([BaseUrl, EncodedParams], "?").

make_get(Twitter) ->
    fun(Path, Args) ->
        get(Twitter, Path, Args)
    end.

-spec make_bearer(#twitter{}) -> #twitter{}.

make_bearer(Twitter) ->
    #twitter{auth=Oauth} = Twitter,
    BearerCreds = oauth:make_bearer_creds(Oauth),
    Headers = [
        {"authorization", lists:append("Basic ", BearerCreds)}],
    ContentType = "application/x-www-form-urlencoded;charset=UTF-8",
    RequestBody = "grant_type=client_credentials",
    case httpc:request(post, {"https://api.twitter.com/oauth2/token",
            Headers, ContentType, RequestBody}, 
            [], [{body_format, binary}]) of
        {ok, Response} ->
            {{_, Status, _}, _, Body} = Response,
            if Status == 200 ->
            % TODO: check token type is "bearer"
                {_, Token} = lists:keyfind(<<"access_token">>, 1, jsx:decode(Body)),
                Twitter#twitter{auth=Oauth#oauth{bearer_token=binary_to_list(Token)}};
            true ->
                {error, Status, Body}
            end;
        Reply ->
            Reply
    end.




