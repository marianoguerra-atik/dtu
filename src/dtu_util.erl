-module(dtu_util).
-export([print_errors_and_warnings/1, print_errors/1, print_warnings/1, node_line/1]).


print_errors_and_warnings(State) ->
    print_warnings(State),
    print_errors(State).

print_warnings(State) ->
    Warns = dtu_walk:get_warnings(State),
    print_msgs(<<"Warning: ">>, Warns).

print_errors(State) ->
    Errors = dtu_walk:get_errors(State),
    print_msgs(<<"Error: ">>, Errors).

node_line(Node) -> element(2, Node).

print_msgs(Prefix, Msgs) ->
    [io:format("~s~p: ~p~n", [Prefix, node_line(Node), Msg]) || {Node, Msg} <- lists:reverse(Msgs)],
    ok.
