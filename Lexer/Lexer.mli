(* Entry point *)

val token : Lexing.lexbuf -> Token.t

(* Standalone lexer for debugging purposes. The string representations
   of the tokens are sent to standard output. *) 

type filename = string

val trace : filename -> unit
