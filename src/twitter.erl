-module(twitter).
-author("Yuce Tekol").

-export([new/1]).

-include("oauth.hrl").
-include("twitter.hrl").


-spec new(#oauth{}) -> #twitter{}.

%% @doc Create new twitter record.
new(Auth) ->
  #twitter{auth = Auth}.
