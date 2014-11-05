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

new(Auth) ->
  new(Auth, []).

new(Auth, Options) when Auth =/= nil ->
  get_twitter_opts(#twitter{auth = Auth}, Options).

get(Twitter, Path, Args) ->
    Uri = make_uri(Twitter, Path, Args),
    case httpc:request(Uri) of
        {ok, _Response}=Reply ->
            Reply;
        {error, _Reason}=Reply ->
            Reply
    end.

make_uri(#twitter{auth=Auth, domain=Domain, api_version=ApiVersion,
            secure=Secure, format=Format}, Path, Args) ->
    Scheme = ?ifte(Secure, "https", "http"),
    BaseUrl = lists:concat([Scheme, "://", Domain, "/", 
        ApiVersion, "/", Path, ".", Format]),
    EncodedParams = oauth:encode_params(Auth, BaseUrl, "GET", Args),
    string:join([BaseUrl, EncodedParams], "?").

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
