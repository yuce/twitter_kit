
-module(twikit_util).
-author("Yuce Tekol").

-export([encode_qry/1, make_url/1, get_timestamp/0]).

-include("util.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

encode_qry({Name, Value}) when is_integer(Value) ->
    encode_qry({Name, integer_to_list(Value)});

encode_qry({Name, Value}) when is_binary(Value) ->
    encode_qry({Name, binary_to_list(Value)});

encode_qry({Name, Value}) when is_atom(Name), is_list(Value)  ->
    string:join([http_uri:encode(atom_to_list(Name)),
                    http_uri:encode(Value)], "=");

encode_qry(Args) ->
    string:join([encode_qry(X) || X <- Args], "&").

make_url({Scheme, Host, Path, QueryString}) ->
    make_url({Scheme, "", Host, 0, Path, QueryString});

make_url({Scheme, _UserInfo, Host, _Port, Path, QueryString}) ->
    BaseUrl = lists:concat([Scheme, '://', Host]),
    make_url({BaseUrl, Path, QueryString});

make_url({BaseUrl, Path, QueryString}) ->
    Path0 = string:left(Path, 1),
    NormPath = ?select(Path0 == "/", Path, lists:append("/", Path)),
    make_url({lists:append(BaseUrl, NormPath), QueryString});

make_url({BaseUrl, QueryString}) ->
    NormQS = ?select(QueryString == "", "",
        lists:append("?", QueryString)),
    lists:append(BaseUrl, NormQS).

-spec get_timestamp() -> seconds().
-type seconds() :: integer().

get_timestamp() ->
  {Mega, Sec, _} = os:timestamp(),
  Mega * 1000000 + Sec .

-ifdef(TEST).

make_url_test() ->
    Target = "https://api.twitter.com/statuses/user_timeline?screen_name=Hurriyet",
    V1 = make_url({"https", "api.twitter.com", "/statuses/user_timeline",
        "screen_name=Hurriyet"}),
    ?assertEqual(V1, Target),

    V2 = make_url({"https://api.twitter.com", "/statuses/user_timeline", 
        "screen_name=Hurriyet"}),
    ?assertEqual(V2, Target),

    V3 = make_url({"https://api.twitter.com/statuses/user_timeline",
        "screen_name=Hurriyet"}),
    ?assertEqual(V3, Target).

encode_qry_test() ->
    Target = "user_id=123&screen_name=some_user&some_bin=test_bin",
    V1 = encode_qry([{user_id, 123}, {screen_name, "some_user"},
                        {some_bin, <<"test_bin">>}]),
    ?assertEqual(V1, Target).

-endif.
