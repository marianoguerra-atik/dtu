-module(dtu_walkers).

-export([unify_ids/3, unify_kws/3, unify_vars/3, unify_strs/3, reject_match/1]).

unify_ids(State0, _Path, {Type, Line, V})
    when Type =:= rid; Type =:= loid; Type =:= upid ->
    {State0, {id, Line, V}};
unify_ids(State0, _Path, Node) ->
    {State0, Node}.

unify_kws(State0, _Path, {Type, Line, V})
    when Type =:= rkw; Type =:= lokw; Type =:= upkw ->
    {State0, {kw, Line, V}};
unify_kws(State0, _Path, Node) ->
    {State0, Node}.

unify_vars(State0, _Path, {Type, Line, V})
    when Type =:= rvar; Type =:= lovar; Type =:= upvar ->
    {State0, {var, Line, V}};
unify_vars(State0, _Path, Node) ->
    {State0, Node}.

unify_strs(State0, _Path, {Type, Line, V})
    when Type =:= dstr; Type =:= bstr; Type =:= sstr ->
    {State0, {str, Line, V}};
unify_strs(State0, _Path, Node) ->
    {State0, Node}.

reject_match(NodeMatcherFn) ->
    fun (State0, _Path, Node) ->
            case NodeMatcherFn(Node) of
              {true, ErrorMsg} ->
                  State1 = dtu_walk:add_error(State0, Node, ErrorMsg),
                  {State1, Node};
              false ->
                  {State0, Node}
            end
    end.

