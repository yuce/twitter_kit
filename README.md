# Twitter Kit

## Introduction

Erlang Twitter API.

Features:

 - Oauth
 - Twitter REST API

Todo:

 - Tweet processing,
 - Streaming API

##Install 

Requires jsx.


## Usage

    Auth = twitter_auth:new({consumer, ?CONSUMER_TOKEN, ?CONSUMER_SECRET}),
    Api = twitter:new(Auth),
    {Pointer, Tweets} = twitter_rest:get(Api, "statuses/user_timeline",
                            [{screen_name, "twitter"},
                             {count, 10}]).
    #twitter_pointer{count=Count} = Pointer,
    io:format("Fetched ~p tweets.~n", [Count]).
    io:format("Tweets: ~p~n", [Tweets]),
    % fetch earlier tweets
    {Pointer2, EarlierTweets} = twitter_rest:get_prev(Pointer).

