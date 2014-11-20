# Twitter Kit

## Introduction

Twitter Kit is an Erlang library for Twitter REST API.

### Features:

 - OAuth authentication,
 - Application (_Bearer_) authentication,
 - Twitter REST API,
 - Cursor and timelines,
 - Media upload,
 - Comprehensive documentation, examples and tests.

### Todo:

 - Following REST endpoints: direct_messages, account, blocks, users, mutes, favorites, lists, saved_searches, geo, trends, application, help
 Streaming API

## Install

Twitter Kit uses [Rebar](https://github.com/rebar/rebar) as its build tool. You just need to add it to your `rebar.config` as a dependency:

    {deps, [
            {twitter_kit, "1.*", {git, "ssh://tiamat.mobilarti.com/git/twitter_kit.git", "master"}}]}.

Twitter Kit uses [jsx](https://github.com/talentdeficit/jsx) to decode JSON responses.

## Usage

Here's a simple example which shows how to traverse user timelines:

    % ConsumerToken and ConsumerSecret are defined somewhere.
    Auth = twitter_auth:new({consumer, ConsumerToken, ConsumerSecret}),
    Api = twitter:new(Auth),
    {ok, {Pointer, Tweets}} = twitter_rest:get(Api, "statuses/user_timeline",
                            [{screen_name, "twitter"},
                             {count, 10}]).
    % Do something with the tweets
    % Fetch earlier tweets
    {ok, {Pointer2, EarlierTweets}} = twitter_rest:get_prev(Pointer).

And another one which shows how to post a tweet with an attached photo:

    % ConsumerToken and ConsumerSecret are defined somewhere.
    Auth = twitter_auth:new({consumer, ConsumerToken, ConsumerSecret}),
    Api = twitter:new(Auth),
    % MediaBinary is the image data, loaded from somewhere, e.g., file system
    {ok, Data} = twitter_media(Api, upload, [{media, "Sample Photo", MediaBinary]}),
    {_, MediaId} = lists:keyfind(<<"media_id">>, 1, Data),
    % Post the tweet
    Text = "This is my funny tweet",
    MediaIdStr = integer_to_list(MediaId),
    {ok, Tweet} = twitter_statuses(Api, update, [{status, Text}, {media_ids, MediaIdStr}]).

There are more examples at XXXX.

## Rationale

Twitter Kit tries to do as little as it can while abstraacting away the boring parts of Twitter API, like authentication and cursors/timelines.

The library is composed of three parts:

1. Authentication functions in `twitter_auth` module,
2. Generic `GET` and `POST` request functions, and cursor/timeline functions in `twitter_rest` module,
3. Helpers in `twitter_followers`, `twitter_friends`, `twitter_media`, `twitter_search`, `twitter_statuses`.



## Documentation
