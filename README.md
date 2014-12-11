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

Twitter Kit is intended to be a helper and supplement to the Twitter API, so you need to consult [Twitter documentation](https://dev.twitter.com/rest/public) for most of the functionality.

Following is the general outline of how the library is used:

1. Create the authentication record using your OAuth or Application info,
2. Use the authentication record to create an API record,
3. Make the call and retrieve the result,
4. Process the result.


### Creating the authentication record

You can use OAuth or Application (_Bearer_) authentication with Twitter Kit. For the former, you can create the authorization record as:

    Auth = twitter_auth:new({consumer, ConsumerKey, ConsumerSecret},
                            {token, TokenKey, TokenSecret}).

and the application authentication as:

    Auth = twitter_auth:new({consumer, ConsumerKey, ConsumerSecret}).


### Creating the API record

Supposing you have the authentication record as `Auth`, it's just:

    Api = twitter:new(Auth).

### Making the API call

There are two ways to make the call: using `twitter_rest` module or the easier `twitter` module which was built on the former. Only the `twitter` module will be described here, you can read the source to see how `twitter_rest` module is used.

The _easy_ API uses the following function structure:

    Result = twitter:METHOD(Api, {API_ENDPOINT_TUPLE}, ENDPOINT_PARAMETERS)

Where, *METHOD* is one of *get* or *post*. *API_ENDPOINT_TUPLE* is a tuple derived from the corresponding API call by replaceing slashes (`/`) with commas (`,`). *ENDPOINT_PARAMETERS* is parameters for the API call, usually a list of tuples

For example, [statuses/update](https://dev.twitter.com/rest/reference/post/statuses/update) API call is used for sending tweets. In this case, *API_ENDPOINT_TUPLE* is `{statuses, update}`. `status` parameter is the one which would have the tweet message, so *ENDPOINT_PARAMETERS*  is [{status, "Our message here"}]. Obviously we have to _post* this call. Here's how you would call it:

    Message = "This is from Twitter Kit",
    {ok, _Tweet} = twitter:post(Api, {statuses, update}, [{status, Message}].

### Process the result

The result of an API call maybe a single object, a list of objects, objects together with a pointer to fetch more objects or an iteration stop atom which tells there are no more objects to fetch.

There are no failure related results. On failures, the process which made the call just crashes, unless you catch the exception.

There are three different kinds of iteration methods in the Twitter API: timelines, cursors and pages. The last one is being obsoleted by Twitter, and other two are supported by this library. Usually, _status_ related methods return a timeline and others return a cursor. In Twitter Kit, all of them are called _pointers_.

Suppose we want to get the Twitter IDs of the 500 followers of @Hurriyet. We need to use the [followers/ids](https://dev.twitter.com/rest/reference/get/followers/ids) endpoint as:

    % @Hurriyet has much more than 500 followers, so I know we will get a pointer.
    % In the general case, we need to check for {stop, _}
    {ok, {Pointer, Ids}} =
            twitter:get(Api, {followers, ids}, [{screen_name, "Hurriyet"}, {count, 500}]),
    {ok, {NewPointer, NewIds}} = twitter:next(Pointer),
    % Handle the rest...

You can see the `user_timeline_archive` example which makes use of pointers.


## More complete examples

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

There are more [example scripts](examples/).

## Examples and tests

There are some [example scripts](examples/) which show how some common tasks are done.

See: [README for tests](test/README.md) for tests.

