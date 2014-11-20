-module(twitter_statuses_tests).
-author("Yuce Tekol").

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").


%% /statuses/user_timeline

get_user_timeline_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun get_user_timeline/1}.


get_user_timeline(_) ->
    Api = get_oauth_api(),
    {ok, {Timeline1, Tweets1}} = twitter:get(Api, {statuses, user_timeline},
                                               [{screen_name, "twitter"}]),
    #twitter_timeline{first_id = First1, last_id = Last1} = Timeline1,
    {ok, {Timeline2, Tweets2}} = twitter:prev(Timeline1),
    #twitter_timeline{first_id = First2, last_id = Last2} = Timeline2,
    {ok, {Timeline3, Tweets3}} = twitter:next(Timeline2),
    #twitter_timeline{first_id = First3, last_id = Last3} = Timeline3,

    [?_assert(First1 < Last1),
     ?_assert(First2 < Last2),
     ?_assert(First3 < Last3),
     ?_assert(First1 > Last2),
     ?_assert(First3 > Last2),
     ?_assert(length(Tweets1) > 0),
     ?_assert(length(Tweets2) > 0),
     ?_assert(length(Tweets3) > 0)].

%%%%
