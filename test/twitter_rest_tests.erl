-module(twitter_rest_tests).
-author("Yuce Tekol").

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").


%% get with app token

get_with_app_token_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun get_with_app_token/1}.

get_with_app_token(_) ->
    Auth = get_app_auth(),
    Tw = twitter:new(Auth),
    {Status, Tweets} =
        twitter_rest:get(Tw, "statuses/user_timeline",
                         [{screen_name, "twitter"}]),
    [?_assertEqual(Status, ok),
     ?_assert(length(Tweets) > 0)].

%%%%
