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
    Get = twitter_rest:make_get(twitter:new(Auth)),
    {ok, Chunk} = Get("statuses/user_timeline", [{screen_name, "twitter"}]).
    #twitter_chunk{count=Count},    
    io:format("Fetched ~p tweets." [Count]).

