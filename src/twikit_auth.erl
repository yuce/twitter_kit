
-module(twikit_auth).
-author("Yuce Tekol").

-export([new/1, new/2]).
-export([make_signed_request/4]).
-export([make_app_request/2, make_app_creds/1]).
-export([obtain_app_auth/1, invalidate_app_auth/1]).

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

oauth2_request(Auth, RequestBody, Path) ->
    AppCreds = make_app_creds(Auth),
    Headers = [{"authorization", lists:append("Basic ", AppCreds)}],
    ContentType = "application/x-www-form-urlencoded;charset=UTF-8",
    Url = twikit_util:make_url({"https://api.twitter.com", Path, []}),
    Request = {Url, Headers, ContentType, RequestBody},
    case httpc:request(post, Request, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, Body};
        {ok, {{_, 403, _}, _, _Body}} ->
            {error, no_permission};
        {ok, {{_, ErrorStatus, _}, _, _Body}} ->
            {error, ErrorStatus};
        {error, Response} ->
            {error, Response}
    end.

obtain_app_auth(Auth) ->
    RequestBody = "grant_type=client_credentials",
    case oauth2_request(Auth, RequestBody, "oauth2/token") of
        {ok, Body} ->
            % TODO: check token type is "bearer"
            {_, Token} = lists:keyfind(<<"access_token">>, 1,
                jsx:decode(Body)),
            {ok, Auth#oauth{app_token=binary_to_list(Token)}};
        {error, Error} ->
            {error, Error}
    end.

invalidate_app_auth(#oauth{app_token=AppToken}=Auth) ->
    RequestBody = string:concat("access_token=", AppToken),
    oauth2_request(Auth, RequestBody, "oauth2/invalidate_token").

-ifdef(TEST).

make_app_creds_test() ->
    Auth = twikit_util:load_term("../test/fixtures/app_pre.fixture"),
    Creds = make_app_creds(Auth),
    TargetValue = twikit_util:load_term("../test/fixtures/make_app_creds_test.fixture"),
    ?assertEqual(Creds, TargetValue).

obtain_app_auth_test() ->
    Auth = twikit_util:load_term("../test/fixtures/app_pre.fixture"),
    {ok, _NewAuth} = obtain_app_auth(Auth),
    lists:foreach(fun(X) -> X:stop() end, [ssl, inets]).

-endif.
