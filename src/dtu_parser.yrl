Nonterminals
    root tl_exprs tl_expr value value_e
    node node_id
    seq seq_item seq_items
    op op_value.

Terminals
    dstr sstr bstr loid upid lokw upkw lovar upvar integer float
    sep colon symbol
    open close
    open_list close_list
    open_map close_map.

Rootsymbol
    root.

root -> tl_exprs     : '$1'.

tl_exprs -> tl_expr : ['$1'].
tl_exprs -> tl_expr sep: ['$1'].
tl_exprs -> tl_expr sep tl_exprs : ['$1'|'$3'].

tl_expr -> op    : '$1'.

node -> node_id seq     : node('$1', '$2', nil).
node -> node_id seq seq : node('$1', '$2', '$3').

node -> node_id open seq_item close :
    node('$1', seq('$2', pseq, ['$3']), nil).
node -> node_id open seq_item close open seq_item close :
    node('$1', seq('$2', pseq, ['$3']), seq('$5', pseq, ['$6'])).
node -> node_id open seq_item close seq :
    node('$1', seq('$2', pseq, ['$3']), '$5').

node -> node_id seq open seq_item close :
    node('$1', '$2', seq('$3', pseq, ['$4'])).

node_id -> loid : '$1'.
node_id -> upid : '$1'.

op -> op_value symbol op : op('$1', '$2', '$3').
op -> op_value : '$1'.

op_value -> value_e : '$1'.

value_e -> value : '$1'.
value_e -> node : '$1'.
value_e -> open tl_expr close : '$2'.

value -> dstr : '$1'.
value -> sstr : '$1'.
value -> bstr : '$1'.

value -> loid : '$1'.
value -> upid : '$1'.

value -> lokw : '$1'.
value -> upkw : '$1'.

value -> lovar : '$1'.
value -> upvar : '$1'.

value -> integer : '$1'.
value -> float : '$1'.

value -> seq : '$1'.

seq -> open close           : seq('$1', pseq, []).
seq -> open_list close_list : seq('$1', lseq, []).
seq -> open_map close_map   : seq('$1', mseq, []).

seq -> open seq_item sep close           : seq('$1', pseq, ['$2']).
seq -> open seq_item sep seq_items close : seq('$1', pseq, ['$2'|'$4']).
seq -> open_list seq_items close_list    : seq('$1', lseq, '$2').
seq -> open_map seq_items close_map      : seq('$1', mseq, '$2').

seq_item -> op : '$1'.
seq_item -> value colon op : pair('$1', '$3').

seq_items -> seq_item : ['$1'].
seq_items -> seq_item sep : ['$1'].
seq_items -> seq_item sep seq_items : ['$1'|'$3'].

Erlang code.

node(Id, Seq, Child) -> {node, line(Id), {Id, Seq, Child}}.

seq(Open, Type, Items) -> {seq, line(Open), {Type, Items}}.

op(Left, Symbol, Right) -> {op, line(Left), {unwrap(Symbol), Left, Right}}.

pair(Left, Right) -> {pair, line(Left), {Left, Right}}.

line(T) when is_tuple(T) -> element(2, T);
line([H|_T]) -> element(2, H);
line(T) -> ct:print("WAT ~p", [T]).

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V;
unwrap(V) -> ct:print("WAT ~p", [V]).
