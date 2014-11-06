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
-export([new/1, new/2, get/3]).

-include("oauth.hrl").
-include("twitter.hrl").

-define(ifte(Cond, True, False), (if Cond -> True; true -> False end)).


-spec new(#oauth{}) -> #twitter{}.

%% @doc Create new twitter record with optional overrides.

new(Auth) ->
  new(Auth, []).

-spec new(#oauth{}, [param()]) -> #twitter{}.
-type param()          :: {param_key(), param_value()}.
-type param_key()      :: atom().
-type param_value()    :: string().

new(Auth, Options) when Auth =/= nil ->
  get_twitter_opts(#twitter{auth = Auth}, Options).

-spec get(#twitter{}, path(), [param()]) -> reply().
-type path() :: atom() | string().
-type reply() :: {ok, list()} | {error, integer()} | {error, any()}.

%% @doc Access Twitter endpoint with Path and Arguments.

get(Twitter, Path, Args) ->
    Uri = make_uri(Twitter, Path, Args),
    case httpc:request(get, {Uri, []}, [], [{body_format, binary}]) of
        {ok, Response} ->
            {{_, Status, _}, _, Body} = Response,
            if Status == 200 -> {ok, jsx:decode(Body)};
                true -> {error, {Status, Body}}
            end;
        {error, _Reason}=Reply ->
            Reply
    end.

-spec make_uri(#twitter{}, path(), [param()]) -> string().

make_uri(#twitter{auth=Auth, domain=Domain, api_version=ApiVersion,
            secure=Secure, format=Format}, Path, Args) ->
    Scheme = ?ifte(Secure, "https", "http"),
    BaseUrl = lists:concat([Scheme, "://", Domain, "/", 
        ApiVersion, "/", Path, ".", Format]),
    EncodedParams = oauth:encode_params(Auth, BaseUrl, "GET", Args),
    string:join([BaseUrl, EncodedParams], "?").

-spec get_twitter_opts(#twitter{}, [param()]) -> #twitter{}.

get_twitter_opts(Twitter, []) ->
  Twitter;

get_twitter_opts(Twitter, [H|T]) ->
  NewTwitter = case H of
    {domain, Domain} ->
      Twitter#twitter{domain = Domain};
    {format, Format} ->
      Twitter#twitter{format = Format};
    {secure, Secure} ->
      Twitter#twitter{secure = (Secure == true)};
    {api_version, ApiVersion} ->
      Twitter#twitter{api_version = ApiVersion};
     _ ->
       Twitter
  end,
  get_twitter_opts(NewTwitter, T).
