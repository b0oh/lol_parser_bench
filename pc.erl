-module(pc).
-export([run/2]).
-export([always/1, bind/2, next/2, last/2, chain/2, either/2, choice/1, many/1, many1/1]).
-export([char/0, char/1, satisfy/1, one_of/1, none_of/1]).

run(P, In) -> P(In).

bind(P, F) ->
    fun(St0) ->
            case P(St0) of
                {ok, X, St1} -> Q = F(X), Q(St1);
                fail -> fail
            end
    end.

next(P, Q) ->
    fun(St0) ->
            case P(St0) of
                {ok, _, St1} -> Q(St1);
                fail -> fail
            end
    end.

last(P, Q) ->
    fun(St0) ->
            case P(St0) of
                {ok, X, St1} ->
                    case Q(St1) of
                        {ok, _, St2} -> {ok, X, St2};
                        fail -> fail
                    end;
                fail -> fail
            end
    end.

chain(Ps, F) -> chain(Ps, F, []).

chain([], F, Acc) -> F(lists:reverse(Acc));
chain([P | Ps], F, Acc) ->
    bind(P, fun(X) -> chain(Ps, F, [X | Acc]) end).

always(X) -> fun(St) -> {ok, X, St} end.

either(P, Q) ->
    fun(St) ->
            case P(St) of
                fail -> Q(St);
                Ok -> Ok
            end
    end.

choice([]) -> fail;
choice([P]) -> P;
choice([P | Ps]) -> either(P, choice(Ps)).

char(C) ->
    fun(<<C_, Rest/binary>>) when C_ == C -> {ok, C_, Rest};
       (_) -> fail
    end.

char() ->
    fun(<<C, Rest/binary>>) -> {ok, C, Rest};
       (_) -> fail
    end.

many(P) -> fun(St0) -> many(P, St0, []) end.

many(P, St0, Acc) ->
    case P(St0) of
        {ok, X, St1} -> many(P, St1, [X | Acc]);
        fail -> {ok, lists:reverse(Acc), St0}
    end.

many1(P) -> chain([P, many(P)], fun([X, Xs]) -> always([X | Xs]) end).

satisfy(F) ->
    fun(<<C, Rest/binary>>) ->
            case F(C) of
                true -> {ok, C, Rest};
                false -> fail
            end;
       (_) -> fail
    end.

one_of(Cs) -> satisfy(fun(C) -> lists:member(C, Cs) end).
none_of(Cs) -> satisfy(fun(C) -> not(lists:member(C, Cs)) end).
