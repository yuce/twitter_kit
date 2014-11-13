
-module(twitter_auth).
-author("Yuce Tekol").

-export([new/1, new/2]).
-export([make_app_request/2]).
-export([obtain_app_auth/1, invalidate_app_auth/1]).

-include("twitter.hrl").
-include("def.hrl").
-include("util.hrl").


-spec new(consumer(), token()) -> #oauth{}.

new({consumer, ConsumerKey, ConsumerSecret},
        {token, TokenKey, TokenSecret}) ->
    #oauth{consumer_key = ConsumerKey,
           consumer_secret = ConsumerSecret,
           token = TokenKey,
           token_secret = TokenSecret}.


-spec new(consumer()) -> #oauth{}.

new({consumer, ConsumerKey, ConsumerSecret}) ->
    #oauth{consumer_key = ConsumerKey,
           consumer_secret = ConsumerSecret}.


-spec make_app_request(#oauth{}, url()) -> request().

make_app_request(#oauth{app_token=BT}, Url) ->
    {Url, [{"authorization", string:concat("Bearer ", BT)}]}.


-spec make_app_creds(#oauth{}) -> string().

make_app_creds(#oauth{consumer_key=Key, consumer_secret=Secret})
        when Key =/= "", Secret =/= "" ->
    base64:encode_to_string(string:join([Key, Secret], ":")).


-spec oauth2_request(#oauth{}, body(), path()) ->
    {ok, binary()} | {error, term()}.

oauth2_request(Auth, RequestBody, Path) ->
    AppCreds = make_app_creds(Auth),
    Headers = [{"authorization", lists:append("Basic ", AppCreds)}],
    ContentType = "application/x-www-form-urlencoded;charset=UTF-8",
    Url = twitter_util:make_url({"https://api.twitter.com", Path, []}),
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


-spec obtain_app_auth(#oauth{}) -> {ok, #oauth{}} | {error, term()}.

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


-spec invalidate_app_auth(#oauth{}) -> {ok, binary()} | {error, term()}.

invalidate_app_auth(#oauth{app_token=AppToken}=Auth) ->
    RequestBody = string:concat("access_token=", AppToken),
    oauth2_request(Auth, RequestBody, "oauth2/invalidate_token").


%% Internal Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

make_app_creds_test() -> 
    TargetValue = twitter_util:load_term("../test/fixtures/make_app_creds_test.fixture"),
    Auth = twitter_util:load_term("../test/fixtures/app_pre.fixture"),
    Creds = make_app_creds(Auth),
    ?_assertEqual(Creds, TargetValue).

-endif.
