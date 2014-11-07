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
