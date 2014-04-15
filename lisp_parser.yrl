Nonterminals exprs expr list.
Terminals fixnum symbol string '(' ')' '\''.
Rootsymbol exprs.

exprs -> expr : ['$1'].
exprs -> expr exprs : ['$1' | '$2'].

expr -> fixnum : '$1'.
expr -> symbol : '$1'.
expr -> string : '$1'.
expr -> '\'' expr : {[quote, '$2'], line('$1')}.
expr -> list : '$1'.

list -> '(' ')' : {[], line('$1')}.
list -> '(' exprs ')' : {'$2', line('$1')}.

Erlang code.

line({_, L}) -> L;
line({_, L, _}) -> L.
