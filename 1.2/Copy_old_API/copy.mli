(* Entry point *)

val get_token : Lexing.lexbuf -> Lexis.token

(* Standalone lexer for debugging purposes. The string representations
   of the tokens are sent to standard output. *) 

type filename = string

val trace : input:filename -> output:filename -> unit
