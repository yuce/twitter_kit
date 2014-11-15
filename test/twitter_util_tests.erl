-module(twitter_util_tests).
-author("Yuce Tekol").

-include_lib("eunit/include/eunit.hrl").


make_url_test() ->
    Target = "https://api.twitter.com/statuses/user_timeline?screen_name=Hurriyet",
    QueryStr = "screen_name=Hurriyet",
    V1 = twitter_util:make_url({"https", "api.twitter.com",
        "/statuses/user_timeline", QueryStr}),
    ?assertEqual(V1, Target),

    V2 = twitter_util:make_url({"https://api.twitter.com",
        "/statuses/user_timeline", QueryStr}),
    ?assertEqual(V2, Target),

    V3 = twitter_util:make_url({"https://api.twitter.com/statuses/user_timeline",
        QueryStr}),
    ?assertEqual(V3, Target).
