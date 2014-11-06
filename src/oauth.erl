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

%-opaque oauth() :: #oauth{}.
%-export_type([oauth/0]).

-spec encode_params(#oauth{}, url(), http_method(), encode_params()) -> encoded_params().
-type encoded_params() :: string().
-type url()            :: string().
-type http_method()    :: string().
-type encode_params()  :: params().
-type param()          :: {param_key(), param_value()}.
-type param_key()      :: atom().
-type param_value()    :: atom() | string() | list() | integer() | float().
-type params()         :: [param()].

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

-spec prepare_params(#oauth{}, encode_params()) -> encode_params().

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

-spec param_encode(param_or_params()) -> string().
-type param_or_params() :: param() | params().

param_encode({Name, Value}) when is_integer(Value) ->
  param_encode({Name, integer_to_list(Value)});

param_encode({Name, Value}) when is_float(Value) ->
  param_encode({Name, float_to_list(Value)});

param_encode({Name, Value}) when is_atom(Name), is_list(Value)  ->
  string:join([url_encode(atom_to_list(Name)), url_encode(Value)], "=");

param_encode(Params) ->
  string:join([param_encode(X) || X <- Params], "&").

-spec get_timestamp() -> seconds().
-type seconds() :: integer().

get_timestamp() ->
  {Mega, Sec, _} = os:timestamp(),
  Mega * 1000000 + Sec .

url_encode(L) ->
  http_uri:encode(L).

