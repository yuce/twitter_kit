-module(oauth).
-author("Yuce Tekol").

-export([percent_encode/1]).
-export([make_post_request/3, make_get_request/3]).

-include("oauth.hrl").
-include("util.hrl").

-define(CRLF, "\r\n").


make_get_request(#oauth{consumer_key=ConsumerKey,
                         consumer_secret=ConsumerSecret,
                         token=AccessToken,
                         token_secret=TokenSecret}, BaseUrl, QueryParams) ->

    OAuthParams = get_oauth_params({consumer_key, ConsumerKey},
                                   {access_token, AccessToken}),
    EncodedQry = encode_qry(get_params(QueryParams, OAuthParams)),
    String = prepare_for_signing("GET", BaseUrl, EncodedQry),
    Key = make_key(ConsumerSecret, TokenSecret),
    Signature = sign_string(Key, String),
    SignedQS = lists:concat([EncodedQry, "&oauth_signature=", 
                             percent_encode(Signature)]),
    {twitter_util:make_url({BaseUrl, SignedQS}), []}.


make_post_request(#oauth{consumer_key=ConsumerKey,
                         consumer_secret=ConsumerSecret,
                         token=AccessToken,
                         token_secret=TokenSecret}, _BaseUrl,
                         {media, FileName, Binary}) ->
    % TODO: Refactor this to use #twitter{upload_domain}
    BaseUrl = "https://upload.twitter.com/1.1/media/upload.json",
    OAuthParams = get_oauth_params({consumer_key, ConsumerKey},
                                   {access_token, AccessToken}),
    EncodedQry = encode_qry(get_params([], OAuthParams)),
    String = prepare_for_signing("POST", BaseUrl, EncodedQry),
    Key = make_key(ConsumerSecret, TokenSecret),
    Signature = sign_string(Key, String),
    AuthParams = get_oauth_params({oauth_signature, Signature}, OAuthParams),
    Header = get_authorization_header(AuthParams),
    make_upload_request(Binary, FileName, "media", [{"Authorization", Header}], BaseUrl);


make_post_request(#oauth{consumer_key=ConsumerKey,
                         consumer_secret=ConsumerSecret,
                         token=AccessToken,
                         token_secret=TokenSecret}, BaseUrl, QueryParams) ->
    OAuthParams = get_oauth_params({consumer_key, ConsumerKey},
                                   {access_token, AccessToken}),
    EncodedQry = encode_qry(get_params(QueryParams, OAuthParams)),
    String = prepare_for_signing("POST", BaseUrl, EncodedQry),
    Key = make_key(ConsumerSecret, TokenSecret),
    Signature = sign_string(Key, String),
    AuthParams = get_oauth_params({oauth_signature, Signature}, OAuthParams),
    Header = get_authorization_header(AuthParams),
    Body = encode_qry(QueryParams),
    make_post_request(BaseUrl, Header, Body);

make_post_request(BaseUrl, AuthHeader, Body) ->
    Headers =[{"Authorization", AuthHeader}],
    ContentType = "application/x-www-form-urlencoded;charset=UTF-8",
    {BaseUrl, Headers, ContentType, Body}.


make_upload_request(Binary, FileName, FieldName, Headers, Url) ->
    BinFileName = list_to_binary(FileName),
    BinFieldName = list_to_binary(FieldName),
    Boundary = <<"9afe66ETK18ae5f">>,  % TODO: random boundary
    Body = <<"--", Boundary/binary, ?CRLF,
             "Content-Disposition: form-data; name=\"", BinFieldName/binary,
                 "\"; filename=\"", BinFileName/binary, "\"", ?CRLF,
             "Content-Type: application/octet-stream", ?CRLF,
             ?CRLF,
             Binary/binary, ?CRLF,
             "--", Boundary/binary, "--", ?CRLF,
             ?CRLF>>,
    ContentType = string:concat("multipart/form-data; boundary=", Boundary),
    {Url, Headers, ContentType, Body}.


get_authorization_header(AuthParams) ->
    F = fun({K, V}) ->
            lists:concat([K, "=", "\"", percent_encode(V), "\""]) end,
    string:concat("OAuth ", string:join(lists:map(F, AuthParams), ", ")).


get_params(ExtraParams, OAuthParams) ->
    lists:append(ExtraParams, OAuthParams).


get_oauth_params({consumer_key, ConsumerKey},
                 {access_token, AccessToken},
                 {nonce, Nonce},
                 {timestamp, Timestamp}) ->
    [{oauth_consumer_key, ConsumerKey},
     {oauth_nonce, Nonce},
     {oauth_signature_method, "HMAC-SHA1"},
     {oauth_timestamp, integer_to_list(Timestamp)},
     {oauth_token, AccessToken},
     {oauth_version, "1.0"}].


get_oauth_params({consumer_key, ConsumerKey},
                 {access_token, AccessToken}) ->
    Nonce = base64:encode_to_string(crypto:rand_bytes(32)),
    Timestamp = get_timestamp(),
    get_oauth_params({consumer_key, ConsumerKey},
                     {access_token, AccessToken},
                     {nonce, Nonce},
                     {timestamp, Timestamp});


get_oauth_params({oauth_signature, _} = Signature, OAuthParams) ->
    lists:sort([Signature|OAuthParams]).


sign_string(Key, String) ->
    base64:encode_to_string(crypto:hmac(sha, list_to_binary(Key),
        list_to_binary(String))).


make_key(ConsumerSecret, TokenSecret) ->
    string:join([percent_encode(ConsumerSecret), 
                 percent_encode(TokenSecret)], "&").


prepare_for_signing(Method, BaseUrl, EncodedParams) ->
    string:join([Method, percent_encode(BaseUrl), 
                         percent_encode(EncodedParams)], "&").


encode_qry({Name, Value}) when is_integer(Value) ->
    encode_qry({Name, integer_to_list(Value)});

% encode_qry({Name, Value}) when is_binary(Value) ->
%     encode_qry({Name, binary_to_list(Value)});

encode_qry({Name, Value}) when is_atom(Name), is_list(Value)  ->
    string:join([percent_encode(atom_to_list(Name)), 
                 percent_encode(Value)], "=");

encode_qry(Args) ->
    encode_qry(Args, "&").

encode_qry(Args, Sep) ->
    string:join(lists:sort([encode_qry(X) || X <- Args]), Sep).


%% TODO: optimize percent_encode

percent_encode(L) ->
    lists:flatten(lists:map(fun encode_char/1, L)).

encode_char(C) when (C >= $0) andalso (C =< $9) -> C;
encode_char(C) when (C >= $A) andalso (C =< $Z) -> C;
encode_char(C) when (C >= $a) andalso (C =< $z) -> C;
encode_char(C) when (C == $-) -> C;
encode_char(C) when (C == $.) -> C;
encode_char(C) when (C == $_) -> C;
encode_char(C) when (C == $~) -> C;

encode_char(C) ->
    L = binary_to_list(unicode:characters_to_binary([C])),
    lists:foldl(fun(X, Acc) -> lists:append(Acc, [$%|integer_to_list(X, 16)]) end, [], L).


get_timestamp() ->
    {Mega, Sec, _} = os:timestamp(),
    Mega * 1000000 + Sec.


%% Internal Tests
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

url_encode_test() ->
    S1 = percent_encode("Ladies + Gentlemen"),
    T1 = "Ladies%20%2B%20Gentlemen",
    ?assertEqual(T1, S1),
    
    S2 = percent_encode("An encoded string!"),
    T2 = "An%20encoded%20string%21",
    ?assertEqual(T2, S2),

    S3 = percent_encode("☃"),
    T3 = "%E2%98%83",
    ?assertEqual(T3, S3),

    S4 = percent_encode("This is Ingénieur études"),
    T4 = "This%20is%20Ing%C3%A9nieur%20%C3%A9tudes",
    ?assertEqual(T4, S4).


get_test_oauth_params() ->
    get_oauth_params({consumer_key, "xvz1evFS4wEEPTGEFPHBog"},
                     {access_token, "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb"},
                     {nonce, "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg"},
                     {timestamp, 1318622958}).
get_test_params() ->
    QueryParams = [{status, "Hello Ladies + Gentlemen, a signed OAuth request!"},
                   {include_entities, "true"}],
    OAuthParams = get_test_oauth_params(),
    get_params(QueryParams, OAuthParams).


encode_qry_test() ->
    Encoded = encode_qry(get_test_params()),
    Target = "include_entities=true&oauth_consumer_key=xvz1evFS4wEEPTGEFPHBog&oauth_nonce=kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg&oauth_signature_method=HMAC-SHA1&oauth_timestamp=1318622958&oauth_token=370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb&oauth_version=1.0&status=Hello%20Ladies%20%2B%20Gentlemen%2C%20a%20signed%20OAuth%20request%21",
    ?assertEqual(Target, Encoded).

prepare_for_signing_test() ->
    EncodedParams = encode_qry(get_test_params()),
    Prepared = prepare_for_signing("POST",
        "https://api.twitter.com/1/statuses/update.json",
        EncodedParams),
    Target = "POST&https%3A%2F%2Fapi.twitter.com%2F1%2Fstatuses%2Fupdate.json&include_entities%3Dtrue%26oauth_consumer_key%3Dxvz1evFS4wEEPTGEFPHBog%26oauth_nonce%3DkYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D1318622958%26oauth_token%3D370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb%26oauth_version%3D1.0%26status%3DHello%2520Ladies%2520%252B%2520Gentlemen%252C%2520a%2520signed%2520OAuth%2520request%2521",
    ?assertEqual(Target, Prepared).

make_key_test() ->
    ConsumerSecret = "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw",
    TokenSecret = "LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE",
    Made = make_key(ConsumerSecret, TokenSecret),
    Target = "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw&LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE",
    ?assertEqual(Target, Made).

sign_string_test() ->
    Key = "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw&LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE",
    String = "POST&https%3A%2F%2Fapi.twitter.com%2F1%2Fstatuses%2Fupdate.json&include_entities%3Dtrue%26oauth_consumer_key%3Dxvz1evFS4wEEPTGEFPHBog%26oauth_nonce%3DkYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D1318622958%26oauth_token%3D370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb%26oauth_version%3D1.0%26status%3DHello%2520Ladies%2520%252B%2520Gentlemen%252C%2520a%2520signed%2520OAuth%2520request%2521",
    Signed = sign_string(Key, String),
    Target = "tnnArxj06cWHq44gCs1OSKk/jLY=",
    ?assertEqual(Target, Signed).

get_authorization_header_test() ->
    OAuthParams = get_test_oauth_params(),
    AuthParams = get_oauth_params({oauth_signature, "tnnArxj06cWHq44gCs1OSKk/jLY="},
                                  OAuthParams),
    Header = get_authorization_header(AuthParams),
    Target = "OAuth oauth_consumer_key=\"xvz1evFS4wEEPTGEFPHBog\", oauth_nonce=\"kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg\", oauth_signature=\"tnnArxj06cWHq44gCs1OSKk%2FjLY%3D\", oauth_signature_method=\"HMAC-SHA1\", oauth_timestamp=\"1318622958\", oauth_token=\"370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb\", oauth_version=\"1.0\"",
    ?assertEqual(Target, Header).

ensure_started() ->
    lists:foreach(fun(X) -> X:start() end, [ssl, inets]).

ensure_stopped() ->
    lists:foreach(fun(X) -> X:stop() end, [inets, ssl]).    

post_request_test() ->
    {ok, [OAuth|_]} = file:consult("test/fixtures/oauth_post.fixture"),
    #oauth{consumer_key = ConsumerKey,
           consumer_secret = ConsumerSecret,
           token = AccessToken,
           token_secret = TokenSecret} = OAuth,
    BaseUrl = "https://api.twitter.com/1.1/statuses/update.json",
    OAuthParams = get_oauth_params({consumer_key, ConsumerKey},
                                   {access_token, AccessToken}),
    QueryParams = [{status, "Maybe he'll quit rororo his keys. #peterfalk"}],
    EncodedQry = encode_qry(get_params(QueryParams, OAuthParams)),
    String = prepare_for_signing("POST", BaseUrl, EncodedQry),
    Key = make_key(ConsumerSecret, TokenSecret),
    Signature = sign_string(Key, String),
    AuthParams = get_oauth_params({oauth_signature, Signature}, OAuthParams),
    Header = get_authorization_header(AuthParams),
    Body = encode_qry(QueryParams),
    Request = make_post_request(BaseUrl, Header, Body),
    % io:format("~p~n", [Request]).
    ensure_started(),
    Response = httpc:request(post, Request, [], [{body_format, binary}]),
    ensure_stopped(),
    io:format("~p~n", [Response]).

post_request_2_test() ->
    {ok, [OAuth|_]} = file:consult("test/fixtures/oauth_post.fixture"),
    BaseUrl = "https://api.twitter.com/1.1/statuses/update.json",
    QueryParams = [{status, "Maybe he'll quit rororo his keys. #peterfalk"}],
    Request = make_post_request(OAuth, BaseUrl, QueryParams),
    ensure_started(),
    Response = httpc:request(post, Request, [], [{body_format, binary}]),
    ensure_stopped(),
    io:format("~p~n", [Response]).

-endif.
