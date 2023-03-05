(* Entry point *)

val get_token : Lexing.lexbuf -> Parser.token

(* Standalone lexer for debugging purposes. The string representations
   of the tokens are sent to standard output. *) 

type filename = string

val trace : filename -> unit

val virt_lnum : int ref
