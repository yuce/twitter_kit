
-module(twitter_util).
-author("Yuce Tekol").

-export([url_encode/1, encode_qry/1, encode_qry/2, make_url/1, get_timestamp/0]).
-export([save_term/2, load_term/1]).

-include("util.hrl").
-include("def.hrl").


-spec encode_qry({key(), integer() | binary() | string()}) -> string()
    ; ([{key(), integer() | binary() | string()}]) -> string().

encode_qry({Name, Value}) when is_integer(Value) ->
    encode_qry({Name, integer_to_list(Value)});

encode_qry({Name, Value}) when is_binary(Value) ->
    encode_qry({Name, binary_to_list(Value)});

encode_qry({Name, Value}) when is_atom(Name), is_list(Value)  ->
    string:join([url_encode(atom_to_list(Name)), url_encode(Value)], "=");

encode_qry(Args) ->
    encode_qry(Args, "&").

encode_qry(Args, Sep) ->
    string:join([encode_qry(X) || X <- Args], Sep).


url_encode(L) ->
    url_encode(L, []).

url_encode([], Acc) ->
    lists:reverse(Acc);

url_encode([H|T], Acc) ->
    case unescaped_char(H) of
        true -> url_encode(T, [H|Acc]);
        _ -> url_encode(T, lists:reverse("%" ++ integer_to_list(H, 16)) ++ Acc)
    end.


unescaped_char(C) when is_integer(C) ->
    if
        (C >= $0) and (C =< $9) -> true;
        (C >= $A) and (C =< $Z) -> true;
        (C >= $a) and (C =< $z) -> true;
        (C == $-) -> true;
        (C == $.) -> true;
        (C == $_) -> true;
        (C == $~) -> true;
        true -> false
    end.


-spec make_url({scheme(), host(), path(), query_string()}) -> url()
    ; ({scheme(), string(), host(), integer(), path(), query_string()})
         -> url()
    ; ({url(), path(), query_string()}) -> url()
    ; ({url(), query_string()}) ->url().

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

get_timestamp() ->
    {Mega, Sec, _} = os:timestamp(),
    Mega * 1000000 + Sec.


-spec save_term(path(), term()) -> ok | {error, term()}.

save_term(Path, Term) ->
    file:write_file(Path, io_lib:fwrite("~p.~n", [Term])).


-spec load_term(path()) -> term().

load_term(Path) ->
    {ok, [Term|_]} = file:consult(Path),
    Term.
