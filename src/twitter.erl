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

-define(VERSION_1_1, "1.1").

%% API
-export([new/1, new/2, get/3]).

-include("oauth.hrl").
-include("twitter.hrl").

-define(POST_ACTIONS, []).

new(Auth) ->
  new(Auth, []).

new(Auth, Options) when Auth =/= nil ->
  get_twitter_opts(#twitter{auth = Auth}, Options).

get(#twitter{auth=Auth, domain=Domain, api_version=ApiVersion, secure=Secure},
    Path, Args) ->
  Proto = case Secure of
            true -> "https";
            _ -> "http"
          end,
  BaseUrl = Proto ++ "://" ++ Domain ++ "/" ++ ApiVersion ++ "/" ++ Path ++ ".json",
  EncodedParams = oauth:encode_params(Auth, BaseUrl, atom_to_list(get), Args),
  Uri =  BaseUrl ++ "?" ++ EncodedParams,
  httpc:request(Uri).

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
