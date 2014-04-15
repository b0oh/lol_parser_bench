-module(bench).
-compile(export_all).

test_avg(M, F, A, N) when N > 0 ->
    L = test_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Range: ~b - ~b mics~n"
          "Median: ~b mics~n"
          "Average: ~b mics~n",
          [Min, Max, Med, Avg]),
    Med.

test_loop(_M, _F, _A, 0, List) ->
    List;
test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, A, N - 1, [T|List]).

run() ->
    test_avg(lol_parse, parse, ["10_thousand.txt"], 1000),
    test_avg(comb, parse, ["10_thousand.txt"], 1000),
    test_avg(erl_tools, parse, ["10_thousand.txt"], 1000).



double(X) -> X * 2.

map0([]) -> [];
map0([X | Xs]) -> [double(X) | map1(Xs)].

map1([]) -> [];
map1([X | Xs]) -> [bench:double(X) | map1(Xs)].

map2(_, []) -> [];
map2(Fun, [X | Xs]) -> [Fun(X) | map2(Fun, Xs)].

map3(_, []) -> [];
map3(Fun, [X | Xs]) -> [apply(Fun, [X]) | map3(Fun, Xs)].

map4(_, _, []) -> [];
map4(Mod, Fun, [X | Xs]) -> [apply(Mod, Fun, [X]) | map4(Mod, Fun, Xs)].

test0(Xs) -> map0(Xs).
test1(Xs) -> map1(Xs).
test2(Xs) -> map2(fun double/1, Xs).
test3(Xs) -> map2(fun bench:double/1, Xs).
test4(Xs) -> map2(fun(X) -> X * 2 end, Xs).
test5(Xs) ->
    Factor = 2,
    Fun = fun(X) -> X * Factor end,
    map2(Fun, Xs).
test6(Xs) -> map3(fun double/1, Xs).
test7(Xs) -> map3(fun bench:double/1, Xs).
test8(Xs) -> map3(fun(X) -> X * 2 end, Xs).
test9(Xs) ->
    Factor = 2,
    Fun = fun(X) -> X * Factor end,
    map3(Fun, Xs).
test10(Xs) -> map4(bench, double, Xs).

funs() ->
    Xs = lists:seq(1, 10000),
    test_avg(bench, test0, [Xs], 10000),
    test_avg(bench, test1, [Xs], 10000),
    test_avg(bench, test2, [Xs], 10000),
    test_avg(bench, test3, [Xs], 10000),
    test_avg(bench, test4, [Xs], 10000),
    test_avg(bench, test5, [Xs], 10000),
    test_avg(bench, test6, [Xs], 10000),
    test_avg(bench, test7, [Xs], 10000),
    test_avg(bench, test8, [Xs], 10000),
    test_avg(bench, test9, [Xs], 10000),
    test_avg(bench, test10, [Xs], 10000).
