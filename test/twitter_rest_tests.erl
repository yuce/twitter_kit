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
    {ok, {Pointer, Tweets}} =
        twitter_rest:get(Tw, "statuses/user_timeline", [{screen_name, "tklx"}]),
    #twitter_pointer{
        first_id = First,
        last_id = Last,
        count = Count} = Pointer,
    [?_assert(Count > 0),
     ?_assertEqual(length(Tweets), Count),
     ?_assert(Last > First)].

%%%%

%% TODO: tests for twitter_rest:get_prev/1 and get_next/1

get_prev_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun get_prev/1}.

get_prev(_) ->    
    Auth = twitter_util:load_term("../test/fixtures/app_post.fixture"),
    Tw = twitter:new(Auth),
    {ok, {Pointer, Tweets}} =
        twitter_rest:get(Tw, "statuses/user_timeline",
                        [{screen_name, "rodneyabrooks"}, {count, 100}]),
    AllTweets = get_archive(Pointer, Tweets, []),
    [?_assert(length(AllTweets) > 200)].

get_archive(_Pointer, [], AllTweets) ->
    AllTweets;

get_archive(Pointer, Tweets, AllTweets) ->
    NewAllTweets = lists:append(Tweets, AllTweets),
    {ok, {NewPointer, NewTweets}} = twitter_rest:get_prev(Pointer),
    get_archive(NewPointer, NewTweets, NewAllTweets).
