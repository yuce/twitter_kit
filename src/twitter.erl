-module(twitter).
-author("Yuce Tekol").

-export([new/1]).

-include("twitter.hrl").
-include("def.hrl").


-spec new(#oauth{}) -> #twitter{}.

new(Auth) ->
     #twitter{auth = Auth}.

