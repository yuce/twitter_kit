
-type request()      :: {url(), headers()} | 
                        {url(), headers(), content_type(), body()}.

-type url()          :: string().
-type headers()      :: [header()].
-type header()       :: {field(), value()}.
-type field()        :: string().
-type value()        :: string().
-type content_type() :: string().

-type body()         :: string() | 
                        binary() | 
                        {fun_acc_a(), accumulator()} |
                        {chunkify, fun_acc_a(), accumulator()}.

-type body_processing_result() :: eof | {ok, iolist(), accumulator()}.
-type accumulator()            :: term().
-type fun_acc_a() :: fun((accumulator()) -> body_processing_result()).
-type method()    :: head | get | put | post | trace | options | delete.
-type query_arg()    :: {key(), string()}.
-type query_args()   :: [query_arg()].
-type key()          :: atom().
-type scheme()       :: string().
-type host()         :: string().
-type path()         :: string().
-type query_string() :: string().
-type seconds()      :: integer().
-type tweet_id()     :: pos_integer().
-type consumer()     :: {consumer, string(), string()}.
-type token()        :: {token, string(), string()}.

-type decode_fun()   :: fun((binary()) -> term()).
-type twitter_options()            :: [] | [twitter_option()].
-type twitter_option()             :: twitter_json_decode_option().
-type twitter_json_decode_option() :: {json_decode, decode_fun()}.
