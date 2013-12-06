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
-export([new/1, new/4, new/5, call/3, call/4]).

-include("oauth.hrl").
-include("twitter.hrl").

-define(POST_ACTIONS, []).

new(Auth) ->
  new(Auth, json, "api.twitter.com", true).

new(Auth, Format, Domain, Secure) ->
  new(Auth, Format, Domain, Secure, ?VERSION_1_1).

new(Auth, json, Domain, Secure, ApiVersion) when Auth =/= nil ->
  #twitter{auth=Auth, domain=Domain, secure=Secure, api_version=ApiVersion}.

call(Twitter, Path, Args) ->
  call(Twitter, get, Path, Args).

call(#twitter{auth=Auth, domain=Domain, api_version=ApiVersion}, Method, Path, Args) ->
  BaseUrl = "https://" ++ Domain ++ "/" ++ ApiVersion ++ "/" ++ Path ++ ".json",
  EncodedParams = oauth:encode_params(Auth, BaseUrl, atom_to_list(Method), Args),
  Uri =  BaseUrl ++ "?" ++ EncodedParams,
  httpc:request(Uri).
