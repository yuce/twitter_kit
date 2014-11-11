-module(twitter_rest_tests).
-author("Yuce Tekol").

-include_lib("eunit/include/eunit.hrl").


get_with_app_token_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun get_with_app_token/1}.

start() ->
    lists:foreach(fun(M) -> M:start() end, [crypto, ssl, inets]).

stop(_) ->
    lists:foreach(fun(M) -> M:stop() end, [inets, ssl]).

get_with_app_token(_) ->
    Auth = twitter_util:load_term("../test/fixtures/app_post.fixture"),
    Tw = twitter:new(Auth),
    Path = "statuses/user_timeline",
    {Status, _Tweets} =
        twitter_rest:get(Tw, Path, [{screen_name, "tklx"}]),
    [?_assertEqual(Status, ok)].
