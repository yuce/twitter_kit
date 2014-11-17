# Twitter Kit

## Introduction

Twitter Kit is an Erlang library for Twitter REST API.

### Features:

 - Oauth authentication,
 - Application (_Bearer_) authentication,
 - Twitter REST API,
 - Cursor and timelines,
 - Photo upload,
 - Comprehensive documentation, examples and tests.

### Todo:

 - Streaming API

## Install

Twitter Kit uses [Rebar](https://github.com/rebar/rebar) as its build tool. You just need to add it to your `rebar.config` as a dependency:

    {deps, [
            {twitter_kit, "1.*", {git, "ssh://tiamat.mobilarti.com/git/twitter_kit.git", "master"}}]}.


Twitter Kit uses [jsx](https://github.com/talentdeficit/jsx) to decode JSON responses.

## Usage

Here's a simple example which shows how to travers user timelines:

    % ConsumerToken and ConsumerSecret are defined somewhere.
    Auth = twitter_auth:new({consumer, ConsumerToken, ConsumerSecret}),
    Api = twitter:new(Auth),
    {Pointer, Tweets} = twitter_rest:get(Api, "statuses/user_timeline",
                            [{screen_name, "twitter"},
                             {count, 10}]).
    % Do something with the tweets
    % Fetch earlier tweets
    {Pointer2, EarlierTweets} = twitter_rest:get_prev(Pointer).

There are more examples at XXXX.

## Documentation
