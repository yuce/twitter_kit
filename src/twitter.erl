-module(twitter).
-author("Yuce Tekol").

-export([new/1, new/2]).

-include("twitter.hrl").
-include("def.hrl").


-spec new(#oauth{}) -> #twitter{}.

new(Auth) ->
     new(Auth, []).


-spec new(#oauth{}, twitter_options()) -> #twitter{}.

new(Auth, Options) ->
    make_twitter(#twitter{auth = Auth}, Options).


-spec make_twitter(#twitter{}, twitter_options()) -> #twitter{}.

make_twitter(Api, []) -> Api;

make_twitter(Api, [{Name, Value}|T]) ->
    case Name of
        json_decode ->
            make_twitter(Api#twitter{json_decode = Value}, T)
    end.


%% Internal Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    Fun = fun(_) -> [] end,
    Api = new(#oauth{}, [{json_decode, Fun}]),
    #twitter{json_decode=JsonDecode} = Api,
    ?assertEqual(JsonDecode, Fun).

-endif.
