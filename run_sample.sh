#! /usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin

main(_Args) ->
    W = sample:home_timeline(),
    io:format("~p~n", [W]).
    %io:format("~n").

