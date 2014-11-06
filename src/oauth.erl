%%%-------------------------------------------------------------------
%%% @author Yuce
%%% @copyright (C) 2013, <Yuce Tekol>
%%% @doc Twitter API for erlang
%%%
%%% @end
%%% Created : 06. Ara 2013 00:44
%%%-------------------------------------------------------------------
-module(oauth).
-author("Yuce").

%% API
-export([encode_params/4, prepare_params/2, get_timestamp/0]).

-include("oauth.hrl").

encode_params(#oauth{token_secret=TokenSecret,
                consumer_secret=ConsumerSecret}=Oauth,
                BaseUrl, Method, Params) ->
  EncParams = param_encode(lists:sort(prepare_params(Oauth, Params))),
  Key = [ConsumerSecret, "&", url_encode(TokenSecret)],
  Message = string:join([url_encode(X) ||
                            X <- [Method, BaseUrl, EncParams]], "&"),
  Signature = base64:encode_to_string(crypto:hmac(sha, list_to_binary(Key),
        list_to_binary(Message))),
  lists:append([EncParams, "&", "oauth_signature=", url_encode(Signature)]).

prepare_params(#oauth{token=Token}=Oauth, Params) when Token =/= nil ->
  [{oauth_token, Token} | prepare_params(Oauth#oauth{token=nil}, Params)];

prepare_params(#oauth{consumer_key=ConsumerKey}, Params) ->
    <<Nonce:32/integer>> = crypto:rand_bytes(4),
    [{oauth_consumer_key, ConsumerKey},
     {oauth_signature_method, "HMAC-SHA1"},
     {oauth_version, "1.0"},
     {oauth_timestamp, integer_to_list(get_timestamp())},
     {oauth_nonce, integer_to_list(Nonce)}
     | Params].

param_encode({Name, Value}) when is_integer(Value) ->
  param_encode({Name, integer_to_list(Value)});

param_encode({Name, Value}) when is_float(Value) ->
  param_encode({Name, float_to_list(Value)});

param_encode({Name, Value}) when is_atom(Name), is_list(Value)  ->
  string:join([url_encode(atom_to_list(Name)), url_encode(Value)], "=");

param_encode(Params) ->
  string:join([param_encode(X) || X <- Params], "&").

get_timestamp() ->
  {Mega, Sec, _} = os:timestamp(),
  Mega * 1000000 + Sec .

%% See https://dev.twitter.com/docs/auth/percent-encoding-parameters

url_encode(L) ->
  http_uri:encode(L).

