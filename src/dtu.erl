-module(dtu).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(["lex_string", String]) ->
    print(lex_string(String));
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    erlang:halt(0).

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

print({ok, Value, _}) ->
    io:format("~p~n", [Value]);
print({error, Reason, Extra}) ->
    io:format("Error: ~p~n    ~p~n", [Reason, Extra]).


%%====================================================================
%% Internal functions
%%====================================================================
