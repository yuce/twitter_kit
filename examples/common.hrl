
-include("../src/twitter.hrl").

start_deps() ->
    lists:foreach(fun(X) -> X:start() end, [ssl, inets]).


get_api() ->
    Auth = twitter_util:load_term("../test/fixtures/oauth_post.fixture"),
    twitter:new(Auth).
