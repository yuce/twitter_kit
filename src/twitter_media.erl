-module(twitter_media).
-author("Yuce Tekol").

-export([post/3]).

-include("twitter.hrl").


post(Api, upload, MediaInfo) ->
    twitter_rest:post(Api, "media/upload", MediaInfo).
