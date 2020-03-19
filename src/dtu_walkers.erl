-module(dtu_walkers).

-export([unify_ids/3,
         unify_kws/3,
         unify_vars/3,
         unify_strs/3,
         pseq_to_tuple/3,
         lseq_to_list/3,
         mseq_to_map/3,
         vars_to_ids/3,
         unwrap_node_id/3,
         reject_match/1]).

pseq_to_tuple(State0, _Path, {seq, Line, {pseq, Items}}) ->
    {State0, {tuple, Line, Items}};
pseq_to_tuple(State0, _Path, Node) ->
    {State0, Node}.

lseq_to_list(State0, _Path, {seq, Line, {lseq, Items}}) ->
    {State0, {list, Line, Items}};
lseq_to_list(State0, _Path, Node) ->
    {State0, Node}.

mseq_to_map(State0, _Path, {seq, Line, {mseq, Items}}) ->
    {State0, {map, Line, Items}};
mseq_to_map(State0, _Path, Node) ->
    {State0, Node}.

unwrap_node_id(State0, _Path, {node, Line, {{id, _, Id}, S1, S2}}) ->
    {State0, {node, Line, {Id, S1, S2}}};
unwrap_node_id(State0, _Path, Node) ->
    {State0, Node}.

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

vars_to_ids(State0, _Path, {Type, Line, V})
    when Type =:= rvar; Type =:= lovar; Type =:= upvar ->
    {State0, {id, Line, <<"$", V/binary>>}};
vars_to_ids(State0, _Path, Node) ->
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

