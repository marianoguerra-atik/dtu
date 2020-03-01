Nonterminals
    value value_e value_e_simple value_anno
    tag
    node node_id node_head node_body
    seq seq_item seq_items seq_empty seq_one seq_multi seq_item_one seq_items_multi
    op op_value
    pair_e
    alt_expr alt_items alt_item alt_item_expr.

Terminals
    dstr sstr bstr
    integer float
    loid upid rid
    lokw upkw rkw
    qloid qupid
    lovar upvar rvar
    lotag uptag rtag
    anno
    sep colon symbol
    open close
    open_list close_list
    open_map close_map
    alt.

Rootsymbol
    op.

node -> node_id alt_expr : node('$1', '$2', empty_seq('$2')).
node -> node_id node_head           : node('$1', '$2', empty_seq('$2')).
node -> node_id node_head node_body : node('$1', '$2', '$3').

node_head -> seq : '$1'.
node_head -> open seq_item close : seq('$1', pseq, ['$2']).

node_body -> seq : '$1'.
node_body -> open seq_item close : seq('$1', pseq, ['$2']).
node_body -> alt_expr : '$1'.

node_id -> loid : '$1'.
node_id -> upid : '$1'.
node_id -> rid : '$1'.

node_id -> qloid : '$1'.
node_id -> qupid : '$1'.

op -> op_value symbol op : op('$1', '$2', '$3').
op -> op_value node_id op : node('$2', ['$1', '$3'], empty_seq('$2')).
op -> op_value : '$1'.

op_value -> value_e : '$1'.
op_value -> symbol value_e: uop('$1', '$2').

value_e -> value_anno : '$1'.
value_e -> tag value_anno : tag('$1', '$2').

tag -> lotag : '$1'.
tag -> uptag : '$1'.
tag -> rtag  : '$1'.

value_anno -> value_e_simple : '$1'.
value_anno -> value_e_simple anno value_e_simple : anno('$1', '$3').

value_e_simple -> value : '$1'.
value_e_simple -> node : '$1'.
value_e_simple -> open op close : '$2'.

value -> dstr : '$1'.
value -> sstr : '$1'.
value -> bstr : '$1'.

value -> loid : '$1'.
value -> upid : '$1'.
value -> rid  : '$1'.

value -> qloid : '$1'.
value -> qupid : '$1'.

value -> lokw : '$1'.
value -> upkw : '$1'.
value -> rkw  : '$1'.

value -> lovar : '$1'.
value -> upvar : '$1'.
value -> rvar  : '$1'.

value -> integer : '$1'.
value -> float : '$1'.

value -> seq : '$1'.

seq -> seq_empty : '$1'.
seq -> seq_one   : '$1'.
seq -> seq_multi : '$1'.

seq_empty -> open close           : seq('$1', pseq, []).
seq_empty -> open_list close_list : seq('$1', lseq, []).
seq_empty -> open_map close_map   : seq('$1', mseq, []).

seq_one -> open seq_item_one close : seq('$1', pseq, ['$2']).
seq_one -> open pair_e close : seq('$1', pseq, ['$2']).
seq_one -> open_list seq_item_one close_list : seq('$1', lseq, ['$2']).
seq_one -> open_list seq_item close_list : seq('$1', lseq, ['$2']).
seq_one -> open_map seq_item_one close_map : seq('$1', mseq, ['$2']).
seq_one -> open_map seq_item close_map : seq('$1', mseq, ['$2']).

seq -> open seq_item sep close           : seq('$1', pseq, ['$2']).
seq -> open seq_item sep seq_items close : seq('$1', pseq, ['$2'|'$4']).

seq_multi -> open seq_items_multi close           : seq('$1', pseq, '$2').
seq_multi -> open_list seq_items_multi close_list : seq('$1', lseq, '$2').
seq_multi -> open_map seq_items_multi close_map   : seq('$1', mseq, '$2').

seq_item -> op : '$1'.
seq_item -> pair_e : '$1'.

pair_e -> value colon op : pair('$1', '$3').

seq_item_one -> seq_item sep : '$1'.

seq_items -> seq_item : ['$1'].
seq_items -> seq_item_one : ['$1'].
seq_items -> seq_items_multi : '$1'.

seq_items_multi -> seq_item sep seq_items : ['$1'|'$3'].

alt_expr -> open alt_items close              : alt('$1', palt, '$2').
alt_expr -> open_list alt_items close_list    : alt('$1', lalt, '$2').
alt_expr -> open_map alt_items close_map      : alt('$1', malt, '$2').

alt_items -> alt_item : ['$1'].
alt_items -> alt_item alt_items : ['$1'|'$2'].

alt_item -> alt alt_item_expr : '$2'. 

alt_item_expr -> op : '$1'.
alt_item_expr -> value_e colon op : pair('$1', '$3').

Erlang code.

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
