-module(dtu_walk).

-export([walk/2,
         walk/4,
         new_state/0,
         compose/1,
         add_warning/3,
         add_error/3,
         get_errors/1,
         get_warnings/1]).

new_state() ->
    #{errors => [], warnings => [], state => #{}}.

add_warning(State = #{warnings := Warns}, Node, Msg) ->
    State#{warnings := [{Node, Msg} | Warns]}.

add_error(State = #{errors := Errors}, Node, Msg) ->
    State#{errors := [{Node, Msg} | Errors]}.

get_errors(#{errors := Errors}) ->
    Errors.

get_warnings(#{warnings := Warns}) ->
    Warns.

run_fns(State0, _Path, Node, []) ->
    {State0, Node};
run_fns(State0, Path, Node, [Fn | Fns]) ->
    {State1, Node1} = Fn(State0, Path, Node),
    run_fns(State1, Path, Node1, Fns).

compose(Fns) ->
    fun (State0, Path, Node) ->
            run_fns(State0, Path, Node, Fns)
    end.

walk(Fn, Node) ->
    walk(new_state(), Fn, [], Node).

walk(State0, Fn, Path, {node, Line, {Id, Seq, Child}}) ->
    Path1 = [node | Path],
    {State1, Id1} = walk(State0, Fn, Path1, Id),
    {State2, Seq1} = walk(State1, Fn, Path1, Seq),
    {State3, Child1} = walk(State2, Fn, Path1, Child),
    Fn(State3, Path, {node, Line, {Id1, Seq1, Child1}});
walk(State0, Fn, Path, {seq, Line, {Type, Items}}) ->
    Path1 = [seq | Path],
    {State1, Items1} = walk_items(State0, Fn, Path1, Items),
    Fn(State1, Path, {seq, Line, {Type, Items1}});
walk(State0, Fn, Path, {op, Line, {Symbol, Left, Right}}) ->
    Path1 = [op | Path],
    {State1, Left1} = walk(State0, Fn, Path1, Left),
    {State2, Right1} = walk(State1, Fn, Path1, Right),
    Fn(State2, Path, {op, Line, {Symbol, Left1, Right1}});
walk(State0, Fn, Path, {uop, Line, {Symbol, Right}}) ->
    Path1 = [uop | Path],
    {State1, Right1} = walk(State0, Fn, Path1, Right),
    Fn(State1, Path, {uop, Line, {Symbol, Right1}});
walk(State0, Fn, Path, {alt, Line, {Type, Items}}) ->
    Path1 = [alt | Path],
    {State1, Items1} = walk_items(State0, Fn, Path1, Items),
    Fn(State1, Path, {alt, Line, {Type, Items1}});
walk(State0, Fn, Path, {pair, Line, {Left, Right}}) ->
    Path1 = [pair | Path],
    {State1, Left1} = walk(State0, Fn, Path1, Left),
    {State2, Right1} = walk(State1, Fn, Path1, Right),
    Fn(State2, Path, {pair, Line, {Left1, Right1}});
walk(State0, Fn, Path, {tag, Line, {Tag, Val}}) ->
    Path1 = [tag | Path],
    {State1, Val1} = walk(State0, Fn, Path1, Val),
    Fn(State1, Path, {tag, Line, {Tag, Val1}});
walk(State0, Fn, Path, {anno, Line, {Val, Anno}}) ->
    Path1 = [anno | Path],
    {State1, Val1} = walk(State0, Fn, Path1, Val),
    {State2, Anno1} = walk(State1, Fn, Path1, Anno),
    Fn(State2, Path, {anno, Line, {Val1, Anno1}});
walk(State0, Fn, Path, {attr, Line, {Key, Args}}) ->
    Path1 = [attr | Path],
    {State1, Args1} = walk_items(State0, Fn, Path1, Args),
    Fn(State1, Path, {attr, Line, {Key, Args1}});
walk(State0, Fn, Path, {attrs, Line, {Attrs, Node}}) ->
    Path1 = [attrs | Path],
    {State1, Attrs1} = walk_items(State0, Fn, Path1, Attrs),
    {State2, Node1} = walk(State1, Fn, Path1, Node),
    Fn(State2, Path, {attrs, Line, {Attrs1, Node1}});
walk(State0, Fn, Path, Node) ->
    Fn(State0, Path, Node).

walk_items(State0, Fn, Path, Items) ->
    walk_items(State0, Fn, Path, Items, []).

walk_items(State0, _Fn, _Path, [], Accum) ->
    {State0, lists:reverse(Accum)};
walk_items(State0, Fn, Path, [H | T], Accum) ->
    {State1, H1} = walk(State0, Fn, Path, H),
    walk_items(State1, Fn, Path, T, [H1 | Accum]).

