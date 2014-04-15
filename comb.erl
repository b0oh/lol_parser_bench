-module(comb).
-export([parse/1]).
-import(pc, [always/1, bind/2, next/2, last/2, chain/2, either/2, choice/1, many/1, many1/1]).
-import(pc, [char/1, satisfy/1, one_of/1, none_of/1]).

letter() -> satisfy(fun(C) -> C >= $a andalso C =< $z orelse C >= $A andalso C =< $Z end).
digit() -> one_of("0123456789").
space() -> one_of("\s\n").
symbol() -> one_of("<>*+=?!:-/").
symbol_init() -> either(letter(), symbol()).
symbol_cont() -> either(symbol_init(), digit()).

parse_fixnum() ->
    bind(many1(digit()), fun(Int) -> always({fixnum, list_to_integer(Int)}) end).

parse_string() ->
    next(char($"),
         last(bind(many(none_of("\"")),
                   fun(S) -> always({string, S}) end),
              char($"))).

symbol_concat([First, Rest]) -> always({symbol, list_to_atom([First | Rest])}).

parse_symbol() ->
    chain([symbol_init(), many(symbol_cont())], fun symbol_concat/1).

parse_quote() ->
    bind(char($'), fun(_) -> bind(parse_expr(), fun(Expr) -> always([quote, Expr]) end) end).

parse_list() ->
    bind(char($(), fun(_) -> last(many(parse_expr()), char($))) end).

parse_expr() ->
    next(many(space()), choice([parse_fixnum(),
                                parse_symbol(),
                                parse_string(),
                                parse_quote(),
                                parse_list()])).


parse(FileName) when is_list(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    parse(Bin);

parse(Bin) when is_binary(Bin) ->
    pc:run(many(parse_expr()), Bin).
