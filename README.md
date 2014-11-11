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
    Twitter = twitter:new(Auth),
    {ok, Tweets} = twitter_rest:get(Twitter,
        "statuses/user_timeline", [{screen_name="twitter"}]).

