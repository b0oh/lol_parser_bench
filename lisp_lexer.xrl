Definitions.

D   = [0-9]
A   = [A-Za-z]
SYM = ({A}|[<>*/+-=?!:-])
WS  = [\s\n\r\t]

Rules.

-?{D}+            : {token, {fixnum, TokenLine, list_to_integer(TokenChars)}}.
{SYM}({SYM}|{D})* : {token, {symbol, TokenLine, list_to_atom(TokenChars)}}.
"[^"]*"           : {token, {string, TokenLine, lists:sublist(TokenChars, 2, TokenLen - 2)}}.
[()']             : {token, {list_to_atom(TokenChars), TokenLine}}.
{WS}              : skip_token.

Erlang code.