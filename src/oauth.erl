
-module(oauth).
-author("Yuce Tekol").

-export([new/1, new/2]).
-export([make_signed_request/4]).
-export([make_app_request/2, make_app_creds/1]).

-include("oauth.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

new({consumer, ConsumerKey, ConsumerSecret},
        {token, TokenKey, TokenSecret}) ->
    #oauth{consumer_key=ConsumerKey,
           consumer_secret=ConsumerSecret,
           token=TokenKey,
           token_secret=TokenSecret}.

new({consumer, ConsumerKey, ConsumerSecret}) ->
    #oauth{consumer_key=ConsumerKey, consumer_secret=ConsumerSecret}.

make_app_request(#oauth{app_token=BT}, Url) ->
    {Url, [{"authorization", lists:concat(["Bearer ", BT])}]}.

make_signed_request(#oauth{token_secret=TS, consumer_secret=CS}=Oauth,
                Method, BaseUrl, QueryArgs) ->
    Key = [CS, "&", http_uri:encode(TS)],
    QS = twikit_util:encode_qry(prepare_args(Oauth, QueryArgs)),
    Signature = make_signature(Key, [Method, BaseUrl, QS]),
    SignedQS = lists:concat([QS, "&", "oauth_signature=",
                 http_uri:encode(Signature)]),
    {twikit_util:make_url({BaseUrl, SignedQS}), []}.
    
make_signature(Key, Items) when is_list(Items) ->
    EncList = lists:map(fun http_uri:encode/1, Items),
    Message = string:join(EncList, "&"),
    base64:encode_to_string(crypto:hmac(sha, list_to_binary(Key),
        list_to_binary(Message))).

prepare_args(#oauth{token=Token, consumer_key=ConsumerKey}, Args)
        when Token =/= "", ConsumerKey =/= "" ->
    <<Nonce:32/integer>> = crypto:rand_bytes(4),
    lists:sort([{oauth_token, Token},
     {oauth_consumer_key, ConsumerKey},
     {oauth_signature_method, "HMAC-SHA1"},
     {oauth_version, "1.0"},
     {oauth_timestamp, integer_to_list(twikit_util:get_timestamp())},
     {oauth_nonce, integer_to_list(Nonce)}
     | Args]).

make_app_creds(#oauth{consumer_key=Key, consumer_secret=Secret})
        when Key =/= "", Secret =/= "" ->
    base64:encode_to_string(string:join([Key, Secret], ":")).

-ifdef(TEST).

make_app_creds_test() ->
    Auth = twikit_util:load_term("../test/fixtures/app_pre.fixture"),
    Creds = make_app_creds(Auth),
    TargetValue = twikit_util:load_term("../test/fixtures/make_app_creds_test.fixture"),
    ?assertEqual(Creds, TargetValue).

-endif.
