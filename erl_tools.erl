-module(erl_tools).
-export([compile_tools/0, parse/1, loop/2]).

compile_tools() ->
    leex:file(lisp_lexer),
    yecc:file(lisp_parser),
    compile:file(lisp_lexer),
    compile:file(lisp_parser),
    ok.

parse(FileName) ->
    {ok, InFile} = file:open(FileName, [read]),
    Acc = loop(InFile, []),
    file:close(InFile),
    lisp_parser:parse(lists:reverse(Acc)).

loop(InFile, Acc) ->
    case io:request(InFile, {get_until, prompt, lisp_lexer, token, [1]}) of
        {ok, Token, _EndLine} ->
            loop(InFile, [Token | Acc]);
        {error, token} ->
            exit(scanning_error);
        {eof, _} ->
            Acc
    end.
