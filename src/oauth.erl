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
-export([encode_params/4, url_encode/1, prepare_params/2, get_timestamp/0, list_to_int/1]).

-include("oauth.hrl").

encode_params(#oauth{token_secret=TokenSecret, consumer_secret=ConsumerSecret}=Oauth,
    BaseUrl, Method, Params) ->
  EncParams = param_encode(lists:sort(prepare_params(Oauth, Params))),
  Key = ConsumerSecret ++ "&" ++ url_encode(TokenSecret),
  Message = string:join([url_encode(X) || X <- [string:to_upper(Method), BaseUrl, EncParams]], "&"),
  Signature = base64:encode_to_string(crypto:hmac(sha, list_to_binary(Key), list_to_binary(Message))),
  EncParams ++ "&" ++ "oauth_signature=" ++ url_encode(Signature).

prepare_params(#oauth{token=Token}=Oauth, Params) when Token =/= nil ->
  [{oauth_token, Token} | prepare_params(Oauth#oauth{token=nil}, Params)];

prepare_params(#oauth{consumer_key=ConsumerKey}, Params) ->
  Params ++ [
    {oauth_consumer_key, ConsumerKey},
    {oauth_signature_method, "HMAC-SHA1"},
    {oauth_version, "1.0"},
    {oauth_timestamp, integer_to_list(get_timestamp())},
    {oauth_nonce, integer_to_list(list_to_int(binary_to_list(crypto:rand_bytes(4))))}
  ].

param_encode({Name, Value}) when is_atom(Name), is_list(Value)  ->
  url_encode(atom_to_list(Name)) ++ "=" ++ url_encode(Value);

param_encode(Params) ->
  string:join([param_encode(X) || X <- Params], "&").

get_timestamp() ->
  {Mega, Sec, _} = os:timestamp(),
  Mega * 1000000 + Sec .

list_to_int(L) ->
  list_to_int(lists:reverse(L), 0, 0).

list_to_int([], Acc, _Idx) ->
  Acc;

list_to_int([H|T], Acc, Idx) ->
  list_to_int(T, Acc + H bsl (Idx * 8), Idx + 1).

%% See https://dev.twitter.com/docs/auth/percent-encoding-parameters

url_encode(L) ->
  url_encode(L, []).

url_encode([], Acc) ->
  lists:reverse(Acc);

url_encode([H|T], Acc) ->
  case unescaped_char(H) of
    true -> url_encode(T, [H|Acc]);
    _ -> url_encode(T, lists:reverse([$%] ++ integer_to_list(H, 16)) ++ Acc)
  end.

unescaped_char(C) when is_integer(C) ->
  if
    (C >= $0) and (C =< $9) -> true;
    (C >= $A) and (C =< $Z) -> true;
    (C >= $a) and (C =< $z) -> true;
    (C == $-) -> true;
    (C == $.) -> true;
    (C == $_) -> true;
    (C == $~) -> true;
    true -> false
  end.
