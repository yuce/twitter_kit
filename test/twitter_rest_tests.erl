-module(twitter_rest_tests).
-author("Yuce Tekol").

-include_lib("eunit/include/eunit.hrl").
-include("../src/twitter.hrl").

%% Common

start() ->
    lists:foreach(fun(M) -> M:start() end, [crypto, ssl, inets]).

stop(_) ->
    lists:foreach(fun(M) -> M:stop() end, [inets, ssl]).


%%%%

get_with_app_token_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun get_with_app_token/1}.

get_with_app_token(_) ->
    Auth = twitter_util:load_term("../test/fixtures/app_post.fixture"),
    Tw = twitter:new(Auth),
    {Status, Tweets} =
        twitter_rest:get(Tw, "statuses/user_timeline", [{screen_name, "tklx"}]),
    [?_assertEqual(Status, ok),
     ?_assert(length(Tweets) > 0)].

%%%%
