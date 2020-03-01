-module(dtu_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 126).

node(Id, Seq, Child) -> {node, line(Id), {Id, Seq, Child}}.

seq(Open, Type, Items) -> {seq, line(Open), {Type, Items}}.

empty_seq(Open) -> seq(Open, lseq, []).

op(Left, Symbol, Right) -> {op, line(Left), {unwrap(Symbol), Left, Right}}.
uop(Symbol, Value) -> {uop, line(Symbol), {unwrap(Symbol), Value}}.

alt(Open, Type, Items) -> {alt, line(Open), {Type, Items}}.

pair(Left, Right) -> {pair, line(Left), {Left, Right}}.

tag(Tag, Val) -> {tag, line(Tag), {Tag, Val}}.

anno(Val, Anno) -> {anno, line(Val), {Val, Anno}}.

line(T) when is_tuple(T) -> element(2, T);
line([H|_T]) -> element(2, H);
line(T) -> ct:print("WAT ~p", [T]).

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V;
unwrap(V) -> ct:print("WAT ~p", [V]).

-file("/home/mariano/bin/erl-22.2/lib/parsetools-2.1.8/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error: Stacktrace ->
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write_atom(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~tp", [Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) -> [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string(Other) ->
    io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("/home/mariano/src/erl/dtu/src/dtu_parser.erl", 202).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, lotag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, rtag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, uptag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_1(S, anno, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value_anno(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_op_value(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value_e(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value_e_simple(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_5/7}).
yeccpars2_5(S, bstr, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, dstr, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, loid, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, lokw, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, lovar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, open_list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, open_map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, rid, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, rkw, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, rvar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, sstr, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, upid, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, upkw, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, upvar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_seq(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_seq(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_seq(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_10(S, symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_op(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_11/7}).
yeccpars2_11(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_11(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_12/7}).
yeccpars2_12(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, open_list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, open_map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value_e_simple(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_18(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, alt, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), alt, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, anno, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), anno, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, close_list, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), close_list, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, close_map, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), close_map, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), sep, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_node_id(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_tag(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_22(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, lotag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, rtag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, uptag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_23(S, close_list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, lotag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, rtag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, uptag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_24(S, close_map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, lotag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, rtag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, uptag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_25(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_S, alt, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), alt, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_S, anno, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), anno, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_S, close_list, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), close_list, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_S, close_map, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), close_map, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), sep, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_S, symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_node_id(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_tag(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_30(S, lotag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, rtag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, uptag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_S, alt, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), alt, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_S, anno, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), anno, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), close, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_S, close_list, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), close_list, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_S, close_map, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), close_map, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), colon, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), sep, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_S, symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), symbol, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_node_id(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_tag(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_op_value(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_36(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_value_e_simple(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_37/7}).
yeccpars2_37(S, close_map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_38/7}).
yeccpars2_38(S, close_map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_39/7}).
yeccpars2_39(S, close_map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_seq_item(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_seq_empty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_42_(Stack),
 yeccgoto_seq_one(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_43(S, bstr, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, dstr, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, loid, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, lokw, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, lotag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, lovar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, open_list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, open_map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, rid, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, rkw, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, rtag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, rvar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, sstr, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, upid, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, upkw, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, uptag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, upvar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_seq_item_one(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_seq_items(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_45_(Stack),
 yeccgoto_seq_items_multi(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_seq_items(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_47(S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_seq_items(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_seq_one(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_seq_multi(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_50: see yeccpars2_0

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_(Stack),
 yeccgoto_seq_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_52/7}).
yeccpars2_52(S, close_list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_52(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_53/7}).
yeccpars2_53(S, close_list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_54/7}).
yeccpars2_54(S, close_list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_55_(Stack),
 yeccgoto_seq_empty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_56_(Stack),
 yeccgoto_seq_one(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_57_(Stack),
 yeccgoto_seq_one(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_58_(Stack),
 yeccgoto_seq_multi(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_59/7}).
yeccpars2_59(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_60/7}).
yeccpars2_60(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_61/7}).
yeccpars2_61(S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_62(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_seq_item(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_63_(Stack),
 yeccgoto_seq_empty(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_64_(Stack),
 yeccgoto_value_e_simple(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_65(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, lotag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, rtag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, uptag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_66/7}).
yeccpars2_66(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_67_(Stack),
 yeccgoto_seq(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_68_(Stack),
 yeccgoto_seq(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_69_(Stack),
 yeccgoto_seq_one(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_70_(Stack),
 yeccgoto_seq_multi(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_node_head(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_72(S, open, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, open_list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, open_map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_72_(Stack),
 yeccgoto_node(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_73_(Stack),
 yeccgoto_node(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_74(S, alt, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(S, lotag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(S, rtag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(S, symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(S, uptag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_75(S, alt, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, close_list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, lotag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, rtag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, uptag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_76(S, alt, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, close_map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, lotag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, rtag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, symbol, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, uptag, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_77/7}).
yeccpars2_77(S, close_map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_78(S, alt, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_78_(Stack),
 yeccgoto_alt_items(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_79: see yeccpars2_0

yeccpars2_80(S, colon, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_op_value(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_alt_item_expr(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_82_(Stack),
 yeccgoto_alt_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_83: see yeccpars2_0

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_84_(Stack),
 yeccgoto_alt_item_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_85_(Stack),
 yeccgoto_alt_items(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_86_(Stack),
 yeccgoto_alt_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_87/7}).
yeccpars2_87(S, close_list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_88_(Stack),
 yeccgoto_alt_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_89/7}).
yeccpars2_89(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_90/7}).
yeccpars2_90(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_91_(Stack),
 yeccgoto_alt_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_92_(Stack),
 yeccgoto_node_head(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_node_body(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_94_(Stack),
 yeccgoto_node(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_node_body(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_96: see yeccpars2_74

-dialyzer({nowarn_function, yeccpars2_97/7}).
yeccpars2_97(S, close, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, sep, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_(Stack),
 yeccgoto_node_body(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_99: see yeccpars2_0

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_100_(Stack),
 yeccgoto_op(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_101_(Stack),
 yeccgoto_value_e(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_102: see yeccpars2_5

yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_103_(Stack),
 yeccgoto_value_anno(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_alt_expr/7}).
yeccgoto_alt_expr(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_alt_expr(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_alt_item/7}).
yeccgoto_alt_item(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_alt_item(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_alt_item(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_alt_item(78, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_alt_item(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_alt_item_expr/7}).
yeccgoto_alt_item_expr(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_alt_items/7}).
yeccgoto_alt_items(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_alt_items(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_alt_items(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(77, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_alt_items(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_alt_items(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_node/7}).
yeccgoto_node(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node(65=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_node_body/7}).
yeccgoto_node_body(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_node_head/7}).
yeccgoto_node_head(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(72, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_node_id/7}).
yeccgoto_node_id(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_id(5, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_id(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_id(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_id(24, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_id(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_id(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_id(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_id(65, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_id(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_id(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_id(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_id(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_id(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_id(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_id(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_id(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_op/7}).
yeccgoto_op(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op(65=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_op_value/7}).
yeccgoto_op_value(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op_value(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op_value(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op_value(24, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op_value(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op_value(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op_value(65, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op_value(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op_value(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op_value(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op_value(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op_value(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op_value(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_op_value(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(10, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_seq/7}).
yeccgoto_seq(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(65=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_seq_empty/7}).
yeccgoto_seq_empty(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(65=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_empty(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_seq_item/7}).
yeccgoto_seq_item(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(61, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_item(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_item(24, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_item(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_item(65, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(47, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_item(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_item(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_item(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(39, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_item(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_seq_item_one/7}).
yeccgoto_seq_item_one(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_item_one(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_item_one(24, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_item_one(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_item_one(65=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_item_one(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_item_one(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_item_one(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_item_one(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_seq_items/7}).
yeccgoto_seq_items(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_items(65, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(66, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_seq_items_multi/7}).
yeccgoto_seq_items_multi(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_items_multi(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_items_multi(24, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_items_multi(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_items_multi(65=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_items_multi(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_items_multi(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(52, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_items_multi(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(37, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_items_multi(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_seq_multi/7}).
yeccgoto_seq_multi(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(65=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_multi(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_seq_one/7}).
yeccgoto_seq_one(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(65=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_seq_one(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_tag/7}).
yeccgoto_tag(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tag(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tag(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tag(24, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tag(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tag(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tag(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tag(65, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tag(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tag(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tag(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tag(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tag(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tag(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_tag(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_value/7}).
yeccgoto_value(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(24, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(65, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_value_anno/7}).
yeccgoto_value_anno(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_anno(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_anno(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_anno(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_anno(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_anno(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_anno(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_anno(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_anno(65=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_anno(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_anno(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_anno(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_anno(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_anno(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_anno(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_anno(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_value_e/7}).
yeccgoto_value_e(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e(65=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e(75=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_value_e_simple/7}).
yeccgoto_value_e_simple(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e_simple(5, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e_simple(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e_simple(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e_simple(24, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e_simple(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e_simple(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e_simple(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e_simple(65, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e_simple(74, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e_simple(75, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e_simple(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e_simple(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e_simple(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e_simple(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e_simple(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value_e_simple(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_35_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 41).
yeccpars2_35_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   uop ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 84).
yeccpars2_41_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   seq ( __1 , mseq , [ ] )
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 90).
yeccpars2_42_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   seq ( __1 , mseq , [ __2 ] )
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 102).
yeccpars2_43_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_45_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 108).
yeccpars2_45_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 105).
yeccpars2_46_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 104).
yeccpars2_47_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 89).
yeccpars2_48_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   seq ( __1 , mseq , [ __2 ] )
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 97).
yeccpars2_49_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   seq ( __1 , mseq , __2 )
  end | __Stack].

-compile({inline,yeccpars2_51_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 100).
yeccpars2_51_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   pair ( __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_55_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 83).
yeccpars2_55_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   seq ( __1 , lseq , [ ] )
  end | __Stack].

-compile({inline,yeccpars2_56_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 88).
yeccpars2_56_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   seq ( __1 , lseq , [ __2 ] )
  end | __Stack].

-compile({inline,yeccpars2_57_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 87).
yeccpars2_57_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   seq ( __1 , lseq , [ __2 ] )
  end | __Stack].

-compile({inline,yeccpars2_58_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 96).
yeccpars2_58_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   seq ( __1 , lseq , __2 )
  end | __Stack].

-compile({inline,yeccpars2_63_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 82).
yeccpars2_63_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   seq ( __1 , pseq , [ ] )
  end | __Stack].

-compile({inline,yeccpars2_64_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 55).
yeccpars2_64_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_67_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 92).
yeccpars2_67_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   seq ( __1 , pseq , [ __2 ] )
  end | __Stack].

-compile({inline,yeccpars2_68_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 93).
yeccpars2_68_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   seq ( __1 , pseq , [ __2 | __4 ] )
  end | __Stack].

-compile({inline,yeccpars2_69_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 86).
yeccpars2_69_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   seq ( __1 , pseq , [ __2 ] )
  end | __Stack].

-compile({inline,yeccpars2_70_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 95).
yeccpars2_70_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   seq ( __1 , pseq , __2 )
  end | __Stack].

-compile({inline,yeccpars2_72_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 23).
yeccpars2_72_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   node ( __1 , __2 , empty_seq ( __2 ) )
  end | __Stack].

-compile({inline,yeccpars2_73_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 22).
yeccpars2_73_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   node ( __1 , __2 , empty_seq ( __2 ) )
  end | __Stack].

-compile({inline,yeccpars2_78_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 114).
yeccpars2_78_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_82_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 117).
yeccpars2_82_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_84_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 120).
yeccpars2_84_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   pair ( __1 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_85_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 115).
yeccpars2_85_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_86_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 112).
yeccpars2_86_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   alt ( __1 , malt , __2 )
  end | __Stack].

-compile({inline,yeccpars2_88_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 111).
yeccpars2_88_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   alt ( __1 , lalt , __2 )
  end | __Stack].

-compile({inline,yeccpars2_91_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 110).
yeccpars2_91_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   alt ( __1 , palt , __2 )
  end | __Stack].

-compile({inline,yeccpars2_92_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 27).
yeccpars2_92_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   seq ( __1 , pseq , [ __2 ] )
  end | __Stack].

-compile({inline,yeccpars2_94_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 24).
yeccpars2_94_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   node ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 30).
yeccpars2_98_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   seq ( __1 , pseq , [ __2 ] )
  end | __Stack].

-compile({inline,yeccpars2_100_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 37).
yeccpars2_100_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   op ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_101_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 44).
yeccpars2_101_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   tag ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_103_/1}).
-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 51).
yeccpars2_103_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   anno ( __1 , __3 )
  end | __Stack].


-file("/home/mariano/src/erl/dtu/src/dtu_parser.yrl", 152).
