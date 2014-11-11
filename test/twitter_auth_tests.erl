-module(twitter_auth_tests).
-author("Yuce Tekol").

-include_lib("eunit/include/eunit.hrl").


obtain_app_auth_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun obtain_app_auth/1}.

start() ->
    lists:foreach(fun(M) -> M:start() end, [crypto, ssl, inets]).

stop(_) ->
    lists:foreach(fun(M) -> M:stop() end, [inets, ssl]).

obtain_app_auth(_) ->
    Auth = twitter_util:load_term("../test/fixtures/app_pre.fixture"),
    {Status, _NewAuth} = twitter_auth:obtain_app_auth(Auth),
    [?_assertEqual(Status, ok)].
