type token =
  | EOF
  | ID of (string)
  | STRING of (string)
  | INT of (int)
  | COMMA
  | COLON
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | DOT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EQ
  | NEQ
  | LT
  | LE
  | GT
  | GE
  | AND
  | OR
  | ASSIGN
  | ARRAY
  | IF
  | THEN
  | ELSE
  | WHILE
  | FOR
  | TO
  | DO
  | LET
  | IN
  | END
  | OF
  | BREAK
  | NIL
  | FUNCTION
  | VAR
  | TYPE

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absyn.exp
