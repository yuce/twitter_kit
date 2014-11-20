-module(twitter_auth_tests).
-author("Yuce Tekol").

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").


obtain_app_auth_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun obtain_app_auth/1}.


obtain_app_auth(_) ->
    Auth = get_app_auth(),
    {Status, #oauth{app_token=AppToken}} = twitter_auth:obtain_app_auth(Auth),
    [?_assertEqual(Status, ok),
     ?_assertNotEqual(AppToken, "")].
