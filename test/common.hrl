
-include("../src/twitter.hrl").


get_app_auth() ->
    twitter_util:load_term("../test/fixtures/app_pre.fixture").


get_oauth_api() ->
    Auth = twitter_util:load_term("../test/fixtures/oauth_post.fixture"),
    twitter:new(Auth).


%% Test setup

start() ->
    lists:foreach(fun(M) -> M:start() end, [ssl, inets]).


stop(_) ->
    lists:foreach(fun(M) -> M:stop() end, [inets, ssl]).

%%%%
