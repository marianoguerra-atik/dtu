-module(dtu).

%% API exports
-export([main/1, ast_file/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(["lex_string", String]) ->
    print(lex_string(String));
main(["ast_string", String]) ->
    print(ast_string(String));
main(["ast_file", Path]) ->
    print(ast_file(Path));
main(["walk_file", Path]) ->
    {ok, Ast} = ast_file(Path),
    Fns = [fun print_node/3],
    WalkFn = dtu_walk:compose(Fns),
    {StateOut, _AstOut} = dtu_walk:walk(WalkFn, Ast),
    dtu_util:print_errors_and_warnings(StateOut);
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    erlang:halt(0).

print_node(State0, Path, Node) ->
    io:format("~p: ~p~n~n", [Path, Node]),
    {State0, Node}.

lex_string(String) ->
    case dtu_lexer:string(String) of
      {ok, Tokens, Endline} ->
          {ok, Tokens, Endline};
      {eof, Endline} ->
          {error, {Endline, dtu_lexer, {eof, Endline}}, []};
      {error, Error} ->
          {error, Error, []};
      {error, Error, Extra} ->
          {error, Error, Extra}
    end.

ast_file(Path) ->
    case file:read_file(Path) of
      {ok, Data} ->
          ast_string(binary_to_list(Data));
      Other ->
          Other
    end.

ast_string(String) ->
    case lex_string(String) of
      {ok, Tokens, _Endline} ->
          dtu_parser:parse(Tokens);
      Other ->
          Other
    end.

print({ok, Value}) ->
    io:format("~p~n", [Value]);
print({ok, Value, _}) ->
    io:format("~p~n", [Value]);
print({error, {Line, dtu_parser, Reason}}) ->
    io:format("Error:~p: ~s~n", [Line, Reason]);
print({error, Reason}) ->
    io:format("Error: ~p~n", [Reason]);
print({error, Reason, Extra}) ->
    io:format("Error: ~p~n    ~p~n", [Reason, Extra]).


%%====================================================================
%% Internal functions
%%====================================================================
