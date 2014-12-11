# Twitter Kit

## Introduction

Twitter Kit is an Erlang library for Twitter REST API.

Twitter Kit tries to do as little as it can while abstracting away the boring parts of Twitter API, like authentication and cursors/timelines.

The library is composed of three parts:

1. Authentication functions in `twitter_auth` module,
2. Generic `GET` and `POST` request functions in `twitter_rest` module,
3. Easy and concise API in the `twitter` module.

You won't need to use `twitter_rest` module unless you need something which is not implemented in `twitter` module.


### Features:

 - OAuth authentication,
 - Application (_Bearer_) authentication,
 - Twitter REST API,
 - Cursor and timelines,
 - Media upload,
 - Comprehensive documentation, examples and tests.

### Todo:

 - Easy API for the following REST endpoints: direct_messages, account, blocks, users, mutes, favorites, lists, saved_searches, geo, trends, application, help
 - Streaming API
 - More tests
 - Test on non-recent Erlang versions and Linux

## Install

Twitter Kit uses [Rebar](https://github.com/rebar/rebar) as its build tool. You just need to add it to your `rebar.config` as a dependency:

    {deps, [
            {twitter_kit, "1.*", {git, "ssh://tiamat.mobilarti.com/git/twitter_kit.git", "master"}}]}.

Twitter Kit uses [jsx](https://github.com/talentdeficit/jsx) to decode JSON responses. You don't need to add it to your config.


## Usage

Here's a simple example which shows how to traverse user timelines:

    % ConsumerToken and ConsumerSecret are defined somewhere.
    Auth = twitter_auth:new({consumer, ConsumerToken, ConsumerSecret}),
    Api = twitter:new(Auth),
    {ok, {Pointer, Tweets}} = twitter:get(Api, {statuses, user_timeline},
                            [{screen_name, "twitter"},
                             {count, 10}]).
    % Do something with the tweets
    % Fetch earlier tweets
    {ok, {Pointer2, EarlierTweets}} = twitter:prev(Pointer).

And another one which shows how to post a tweet with an attached photo:

    % ConsumerToken and ConsumerSecret are defined somewhere.
    Auth = twitter_auth:new({consumer, ConsumerToken, ConsumerSecret}),
    Api = twitter:new(Auth),
    % MediaBinary is the image data, loaded from somewhere, e.g., file system
    {ok, Data} = twitter(Api, {media, upload}, {media, "Sample Photo", MediaBinary}),
    {_, MediaId} = lists:keyfind(<<"media_id">>, 1, Data),
    % Post the tweet
    Text = "This is my funny tweet",
    MediaIdStr = integer_to_list(MediaId),
    {ok, Tweet} = twitter(Api, {statuses, update}, [{status, Text}, {media_ids, MediaIdStr}]).

See the [wiki](wiki/Usage) for more documentation.

## Examples and tests

There are some [example scripts](examples/) which show how some common tasks are done.

See: [README for tests](test/README.md).

