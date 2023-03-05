{
(* Lexer specification of C# 1.2 for ocamllex *)

(* Copying the matched prefix of the input buffer
   to standard output *)

let copy lexbuf = print_string (Lexing.lexeme lexbuf)

(* String processing *)

let mk_str (len:int) (p: char list) : string =
  let s = Bytes.make len ' ' in 
  let rec fill i =
    function [] -> s | c::l -> Bytes.set s i c; fill (i-1) l
in assert (len = List.length p); Bytes.to_string (fill (len-1) p)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i-1) (s.[i]::l)
in exp (String.length s - 1)

(* Virtual line number (according to #line) and end of lines *)

let virt_lnum = ref 1

let handle_nl buffer = Lexing.new_line buffer; incr virt_lnum

(* Error handling *)

let fail msg buffer =
  raise (Error.(Lexer (msg, mk_seg buffer, !virt_lnum)))

let halt msg buffer =
  let pos = buffer.Lexing.lex_curr_p
in raise (Error.(Lexer (msg, (pos, pos), !virt_lnum)))

exception Local_err of Error.message

let handle_err scan buffer =
  try scan buffer with Local_err msg -> fail msg buffer

(* Concrete syntax of tokens.
   
   See module [Lexis] for abstract syntax.
*)

let string_of_token = 
  let open Lexis in function
  (* Keywords *)

  ABSTRACT   -> "abstract"
| AS         -> "as"
| BASE       -> "base"
| BOOL       -> "bool"
| BREAK      -> "break"
| BYTE       -> "byte"
| CASE       -> "case"
| CATCH      -> "catch"
| CHAR       -> "char"
| CHECKED    -> "checked"
| CLASS      -> "class"
| CONST      -> "const"
| CONTINUE   -> "continue"
| DECIMAL    -> "decimal"
| DEFAULT    -> "default"
| DELEGATE   -> "delegate"
| DO         -> "do"
| DOUBLE     -> "double"
| ELSE       -> "else"
| ENUM       -> "enum"
| EVENT      -> "event"
| EXPLICIT   -> "explicit"
| EXTERN     -> "extern"
| FALSE      -> "false"
| FINALLY    -> "finally"
| FIXED      -> "fixed"
| FLOAT      -> "float"
| FOR        -> "for"
| FOREACH    -> "foreach"
| GOTO       -> "goto"
| IF         -> "if"
| IMPLICIT   -> "implicit"
| IN         -> "in"
| INT        -> "int"
| INTERFACE  -> "interface"
| INTERNAL   -> "internal"
| IS         -> "is"
| LOCK       -> "lock"
| LONG       -> "long"
| NAMESPACE  -> "namespace"
| NEW        -> "new"
| NULL       -> "null"
| OBJECT     -> "object"
| OPERATOR   -> "operator"
| OUT        -> "out"
| OVERRIDE   -> "override"
| PARAMS     -> "params"
| PRIVATE    -> "private"
| PROTECTED  -> "protected"
| PUBLIC     -> "public"
| READONLY   -> "readonly"
| REF        -> "ref"
| RETURN     -> "return"
| SBYTE      -> "sbyte"
| SEALED     -> "sealed"
| SHORT      -> "short"
| SIZEOF     -> "sizeof"
| STACKALLOC -> "stackalloc"
| STATIC     -> "static"
| STRING     -> "string"
| STRUCT     -> "struct"
| SWITCH     -> "switch"
| THIS       -> "this"
| THROW      -> "throw"
| TRUE       -> "true"
| TRY        -> "try"
| TYPEOF     -> "typeof"
| UINT       -> "uint"
| ULONG      -> "ulong"
| UNCHECKED  -> "unchecked"
| UNSAFE     -> "unsafe"
| USHORT     -> "ushort"
| USING      -> "using"
| VIRTUAL    -> "virtual"
| VOID       -> "void"
| VOLATILE   -> "volatile"
| WHILE      -> "while"

(* Symbols and operators *)

| LBRACE    -> "{"
| RBRACE    -> "}"
| LBRACKET  -> "["
| RBRACKET  -> "]"
| LPAR      -> "("
| RPAR      -> ")"
| DOT       -> "."
| COMMA     -> ","
| COLON     -> ":"
| SEMI      -> ";"

| PLUS      -> "+"
| MINUS     -> "-"
| TIMES     -> "*"
| SLASH     -> "/"
| PERCENT   -> "%"
| AMPER     -> "&"
| MID       -> "|"
| CIRCUM    -> "^"
| BANG      -> "!"
| TILDE     -> "~"

| ASSIGN    -> "="
| LT        -> "<"
| GT        -> ">"
| QMARK     -> "?"
| DPLUS     -> "++"
| DMINUS    -> "--"
| DAMPER    -> "&&"
| DMID      -> "||"
| LSHIFT    -> "<<"
| RSHIFT    -> ">>"

| EQ        -> "=="
| NE        -> "!="
| LEQ       -> "<="
| GEQ       -> ">="
| PLUSEQ    -> "+="
| MINUSEQ   -> "-="
| TIMESEQ   -> "*="
| DIVEQ     -> "/="
| PEREQ     -> "%="
| AMPEREQ   -> "&="

| MIDEQ     -> "|="
| CIREQ     -> "^="
| LSHIFTEQ  -> "<<="
| RSHIFTEQ  -> ">>="
| ARROW     -> "->"

(* Literals *)

| Ident (Norm,s)  -> "Ident(" ^ s ^ ")"
| Ident (Verb,s)  -> "Ident(@" ^ s ^ ")"
| Int s           -> "Int(" ^ s ^ ")"
| Float s         -> "Float(" ^ s ^ ")" 
| Char s          -> "Char(" ^ s ^ ")"
| String (Norm,s) -> "String(\"" ^ s ^ "\")"
| String (Verb,s) -> "String(@\"" ^ s ^ "\")"

(* Virtual tokens *)

| EOF  -> "EOF" (* End of File *)
| CAST -> "CAST"

(* Deterministic finite automaton to recognise some C# casts and emit
   the virtual token CAST in prefix position accordingly, in order to
   solve a conflict in the parser between expressions (multiplication
   and dereferencing) and casts. *)

type streamlined = Lexing.lexbuf -> Lexis.token * 'a as 'a

let rec pad (lexer: streamlined) = function
             [] -> assert false
|       [token] -> token, lexer
| token::tokens -> token, fun _ -> pad lexer tokens 

let mk_cast tokens lexer = pad lexer (Lexis.CAST :: List.rev tokens)

let tokenise tokens lexer = pad lexer (List.rev tokens)

let chk_cast tokens lexer buffer =
  let open Lexis in
  let token, lexer' = lexer buffer in
  let tokens' = token::tokens
in match token with
     LPAR | CAST | Ident _ -> mk_cast tokens' lexer'
   |                     _ -> tokenise tokens' lexer'

let rec after_LPAR (lexer: streamlined) (buffer: Lexing.lexbuf) =
  let open Lexis in
  let token, lexer' = lexer buffer in
  let tokens' = [token;LPAR]
in match token with
     SBYTE | BYTE | SHORT | USHORT | INT | UINT | LONG 
   | ULONG | CHAR | FLOAT | DOUBLE | DECIMAL | BOOL
   | OBJECT | STRING -> after_kwd tokens' lexer' buffer
   |         Ident _ -> after_id tokens' lexer' buffer
   |               _ -> tokenise tokens' lexer'

and after_id tokens lexer buffer =
  let open Lexis in
  let token, lexer' = lexer buffer in
  let tokens' = token::tokens
in match token with
         RPAR -> chk_cast tokens' lexer' buffer
   |    TIMES -> after_times tokens' lexer' buffer
   | LBRACKET -> after_fst_lbr tokens' lexer' buffer
   |      DOT -> after_dot tokens' lexer' buffer
   |        _ -> tokenise tokens' lexer'

and after_kwd tokens lexer buffer =
  let open Lexis in
  let token, lexer' = lexer buffer in
  let tokens' = token::tokens
in match token with
         RPAR -> mk_cast tokens' lexer'
   |    TIMES -> after_times tokens' lexer' buffer
   | LBRACKET -> after_fst_lbr tokens' lexer' buffer
   |        _ -> tokenise tokens' lexer'

and after_dot tokens lexer buffer =
  let open Lexis in
  let token, lexer' = lexer buffer in
  let tokens' = token::tokens
in match token with
     Ident _ -> after_id tokens' lexer' buffer
   |       _ -> tokenise tokens' lexer'

and after_times tokens lexer buffer =
  let open Lexis in
  let token, lexer' = lexer buffer in
  let tokens' = token::tokens
in match token with
        TIMES -> after_times tokens' lexer' buffer
   |     RPAR -> mk_cast tokens' lexer'
   | LBRACKET -> after_fst_lbr tokens' lexer' buffer
   |        _ -> tokenise tokens' lexer'

and after_fst_lbr tokens lexer buffer =
  let open Lexis in
  let token, lexer' = lexer buffer in
  let tokens' = token::tokens
in match token with
        COMMA -> after_fst_lbr tokens' lexer' buffer
   | RBRACKET -> after_more_lbr tokens' lexer' buffer
   |        _ -> tokenise tokens' lexer'

and after_more_lbr tokens lexer buffer =
  let open Lexis in
  let token, lexer' = lexer buffer in
  let tokens' = token::tokens
in match token with
     LBRACKET -> after_fst_lbr tokens' lexer' buffer
   |     RPAR -> mk_cast tokens' lexer'
   |        _ -> tokenise tokens' lexer'

(* Miscellanea *)

let trace tokens =
  prerr_endline "START TRACE";
  List.iter (fun t -> prerr_endline (string_of_token t)) (List.rev tokens); 
  prerr_endline "END TRACE"

let repeat t n (lexer: Lexing.lexbuf -> Lexis.token * 'a as 'a) =
  assert (n > 0);
  let rec make n : Lexis.token * 'a =
    t, if n = 1 then lexer else fun _ -> make (n-1)
in make n

let rec rm_cast = function
                  [] -> []
| Lexis.CAST::tokens -> rm_cast tokens
|      token::tokens -> token :: rm_cast tokens

}

(* Regular expressions for literals *)

(* White space *)

let newline = '\n' | '\r' | "\r\n"
let blank = ' ' | '\t'

(* Integers *)

let int_suf = 'U' | 'u' | 'L' | 'l' | "UL" | "Ul" | "uL"
            | "ul" | "LU" | "Lu" | "lU" | "lu"
let digit = ['0'-'9']
let dec = digit+ int_suf?
let hexdigit = digit | ['A'-'F' 'a'-'f']
let hex_pre = "0x" | "0X"
let hexa = hex_pre hexdigit+ int_suf?
let integer = dec | hexa

(* Unicode escape sequences *)

let four_hex = hexdigit hexdigit hexdigit hexdigit
let uni_esc = "\\u" four_hex | "\\U"  four_hex four_hex

(* Identifiers *)

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letter = lowercase | uppercase | uni_esc
let start = '_' | letter
let alphanum = letter | digit | '_'
let ident = start alphanum*

(* Real *)

let decimal = digit+
let exponent = ['e' 'E'] ['+' '-']? decimal
let real_suf = ['F' 'f' 'D' 'd' 'M' 'm']
let real = (decimal? '.')? decimal exponent? real_suf?

(* Characters *)

let single = [^ '\n' '\r']
let esc = "\\'" | "\\\"" | "\\\\" | "\\0" | "\\a" | "\\b" | "\\f"
        | "\\n" | "\\r" | "\\t" | "\\v"
let hex_esc = "\\x" hexdigit hexdigit? hexdigit? hexdigit?
let character = single | esc | hex_esc | uni_esc
let char = "'" character "'"

(* Rules *)

(* val token : Lexing.lexbuf -> Lexis.token * 'a as 'a *)

rule token = parse
  newline      { handle_nl lexbuf; copy lexbuf; token lexbuf }
| blank+       { copy lexbuf; token lexbuf }

| "abstract"   { copy lexbuf; Lexis.ABSTRACT,   token }
| "as"         { copy lexbuf; Lexis.AS,         token }
| "base"       { copy lexbuf; Lexis.BASE,       token }
| "bool"       { copy lexbuf; Lexis.BOOL,       token }
| "break"      { copy lexbuf; Lexis.BREAK,      token }
| "byte"       { copy lexbuf; Lexis.BYTE,       token }
| "case"       { copy lexbuf; Lexis.CASE,       token }
| "catch"      { copy lexbuf; Lexis.CATCH,      token }
| "char"       { copy lexbuf; Lexis.CHAR,       token }
| "checked"    { copy lexbuf; Lexis.CHECKED,    token }
| "class"      { copy lexbuf; Lexis.CLASS,      token }
| "const"      { copy lexbuf; Lexis.CONST,      token }
| "continue"   { copy lexbuf; Lexis.CONTINUE,   token }
| "decimal"    { copy lexbuf; Lexis.DECIMAL,    token }
| "default"    { copy lexbuf; Lexis.DEFAULT,    token }
| "delegate"   { copy lexbuf; Lexis.DELEGATE,   token }
| "do"         { copy lexbuf; Lexis.DO,         token }
| "double"     { copy lexbuf; Lexis.DOUBLE,     token }
| "else"       { copy lexbuf; Lexis.ELSE,       token }
| "enum"       { copy lexbuf; Lexis.ENUM,       token }
| "event"      { copy lexbuf; Lexis.EVENT,      token }
| "explicit"   { copy lexbuf; Lexis.EXPLICIT,   token }
| "extern"     { copy lexbuf; Lexis.EXTERN,     token }
| "false"      { copy lexbuf; Lexis.FALSE,      token }
| "finally"    { copy lexbuf; Lexis.FINALLY,    token }
| "fixed"      { copy lexbuf; Lexis.FIXED,      token }
| "float"      { copy lexbuf; Lexis.FLOAT,      token }
| "for"        { copy lexbuf; Lexis.FOR,        token }
| "foreach"    { copy lexbuf; Lexis.FOREACH,    token }
| "goto"       { copy lexbuf; Lexis.GOTO,       token }
| "if"         { copy lexbuf; Lexis.IF,         token }
| "implicit"   { copy lexbuf; Lexis.IMPLICIT,   token }
| "in"         { copy lexbuf; Lexis.IN,         token }
| "int"        { copy lexbuf; Lexis.INT,        token }
| "interface"  { copy lexbuf; Lexis.INTERFACE,  token }
| "internal"   { copy lexbuf; Lexis.INTERNAL,   token }
| "is"         { copy lexbuf; Lexis.IS,         token }
| "lock"       { copy lexbuf; Lexis.LOCK,       token }
| "long"       { copy lexbuf; Lexis.LONG,       token }
| "namespace"  { copy lexbuf; Lexis.NAMESPACE,  token }
| "new"        { copy lexbuf; Lexis.NEW,        token }
| "null"       { copy lexbuf; Lexis.NULL,       token }
| "object"     { copy lexbuf; Lexis.OBJECT,     token }
| "operator"   { copy lexbuf; Lexis.OPERATOR,   token }
| "out"        { copy lexbuf; Lexis.OUT,        token }
| "override"   { copy lexbuf; Lexis.OVERRIDE,   token }
| "params"     { copy lexbuf; Lexis.PARAMS,     token }
| "private"    { copy lexbuf; Lexis.PRIVATE,    token }
| "protected"  { copy lexbuf; Lexis.PROTECTED,  token }
| "public"     { copy lexbuf; Lexis.PUBLIC,     token }
| "readonly"   { copy lexbuf; Lexis.READONLY,   token }
| "ref"        { copy lexbuf; Lexis.REF,        token }
| "return"     { copy lexbuf; Lexis.RETURN,     token }
| "sbyte"      { copy lexbuf; Lexis.SBYTE,      token }
| "sealed"     { copy lexbuf; Lexis.SEALED,     token }
| "short"      { copy lexbuf; Lexis.SHORT,      token }
| "sizeof"     { copy lexbuf; Lexis.SIZEOF,     token }
| "stackalloc" { copy lexbuf; Lexis.STACKALLOC, token }
| "static"     { copy lexbuf; Lexis.STATIC,     token }
| "string"     { copy lexbuf; Lexis.STRING,     token }
| "struct"     { copy lexbuf; Lexis.STRUCT,     token }
| "switch"     { copy lexbuf; Lexis.SWITCH,     token }
| "this"       { copy lexbuf; Lexis.THIS,       token }
| "throw"      { copy lexbuf; Lexis.THROW,      token }
| "true"       { copy lexbuf; Lexis.TRUE,       token }
| "try"        { copy lexbuf; Lexis.TRY,        token }
| "typeof"     { copy lexbuf; Lexis.TYPEOF,     token }
| "uint"       { copy lexbuf; Lexis.UINT,       token }
| "ulong"      { copy lexbuf; Lexis.ULONG,      token }
| "unchecked"  { copy lexbuf; Lexis.UNCHECKED,  token }
| "unsafe"     { copy lexbuf; Lexis.UNSAFE,     token }
| "ushort"     { copy lexbuf; Lexis.USHORT,     token }
| "using"      { copy lexbuf; Lexis.USING,      token }
| "virtual"    { copy lexbuf; Lexis.VIRTUAL,    token }
| "void"       { copy lexbuf; Lexis.VOID,       token }
| "volatile"   { copy lexbuf; Lexis.VOLATILE,   token }
| "while"      { copy lexbuf; Lexis.WHILE,      token }

| '{'          { copy lexbuf; Lexis.LBRACE,     token }
| '}'          { copy lexbuf; Lexis.RBRACE,     token }
| '['          { copy lexbuf; Lexis.LBRACKET,   token }
| ']'          { copy lexbuf; Lexis.RBRACKET,   token }
| '('          { copy lexbuf; after_LPAR token lexbuf  }
| ')'          { copy lexbuf; Lexis.RPAR,       token }
| '.'          { copy lexbuf; Lexis.DOT,        token }
| ','          { copy lexbuf; Lexis.COMMA,      token }
| ':'          { copy lexbuf; Lexis.COLON,      token }
| ';'          { copy lexbuf; Lexis.SEMI,       token }

| '+'          { copy lexbuf; Lexis.PLUS,       token }
| '-'          { copy lexbuf; Lexis.MINUS,      token }
| '*'          { copy lexbuf; Lexis.TIMES,      token }
| '/'          { copy lexbuf; Lexis.SLASH,      token }
| '%'          { copy lexbuf; Lexis.PERCENT,    token }
| '&'          { copy lexbuf; Lexis.AMPER,      token }
| '|'          { copy lexbuf; Lexis.MID,        token }
| '^'          { copy lexbuf; Lexis.CIRCUM,     token }
| '!'          { copy lexbuf; Lexis.BANG,       token }
| '~'          { copy lexbuf; Lexis.TILDE,      token }

| '='          { copy lexbuf; Lexis.ASSIGN,     token }
| '<'          { copy lexbuf; Lexis.LT,         token }
| '>'          { copy lexbuf; Lexis.GT,         token }
| '?'          { copy lexbuf; Lexis.QMARK,      token }
| "++"         { copy lexbuf; Lexis.DPLUS,      token }
| "--"         { copy lexbuf; Lexis.DMINUS,     token }
| "&&"         { copy lexbuf; Lexis.DAMPER,     token }
| "||"         { copy lexbuf; Lexis.DMID,       token }
| "<<"         { copy lexbuf; Lexis.LSHIFT,     token }
| ">>"         { copy lexbuf; Lexis.RSHIFT,     token }

| "=="         { copy lexbuf; Lexis.EQ,         token }
| "!="         { copy lexbuf; Lexis.NE,         token }
| "+="         { copy lexbuf; Lexis.PLUSEQ,     token }
| "-="         { copy lexbuf; Lexis.MINUSEQ,    token }
| "*="         { copy lexbuf; Lexis.TIMESEQ,    token }
| "/="         { copy lexbuf; Lexis.DIVEQ,      token }
| "%="         { copy lexbuf; Lexis.PEREQ,      token }
| "&="         { copy lexbuf; Lexis.AMPEREQ,    token }
| "|="         { copy lexbuf; Lexis.MIDEQ,      token }
| "^="         { copy lexbuf; Lexis.CIREQ,      token }

| "<<="        { copy lexbuf; Lexis.LSHIFTEQ,   token }
| "<="         { copy lexbuf; Lexis.LEQ,        token }
| ">>="        { copy lexbuf; Lexis.RSHIFTEQ,   token }
| ">="         { copy lexbuf; Lexis.GEQ,        token }

| "->"         { copy lexbuf; Lexis.ARROW,      token }

| integer as s     { copy lexbuf; Lexis.Int s,         token }
| real as s        { copy lexbuf; Lexis.Float s,       token }
| ident as s       { copy lexbuf; Lexis.Ident(Norm,s), token }
| '@' (ident as s) { copy lexbuf; Lexis.Ident(Verb,s), token }
| char as s        { copy lexbuf; Lexis.Char s,        token }

| '"'   { copy lexbuf; handle_err (in_norm_str 0 []) lexbuf }
| "@\"" { copy lexbuf; handle_err (in_verb_str 0 []) lexbuf }

| "//" { copy lexbuf; handle_err in_line_com lexbuf; token lexbuf  }
| "/*" { copy lexbuf; handle_err in_block_com lexbuf; token lexbuf }

| eof { copy lexbuf; Lexis.EOF, token }

| '#' blank* "line" blank* (newline | eof) {
    halt "Line indicator expected." lexbuf }
| '#' blank* "line" blank+ { copy lexbuf; line_indicator lexbuf; token lexbuf }

(* This disjunct has to be reworked to accept UTF-8 characters.
   In the meantime, we skip all UTF-8 characters.
 
| _ as c { let open Error in
           let code = Char.code c in
           let message = "Invalid character " ^ String.make 1 c 
                         ^ " (" ^ string_of_int code ^ ")."
           in raise (Lexer (message, mk_seg lexbuf)) }
*)

| _ { copy lexbuf; token lexbuf }

(* Line indicator (#line) *)

and line_indicator = parse
  decimal as n {
    end_indicator lexbuf;
    virt_lnum := int_of_string n
  }
| ident as id {
    match id with
      "default" -> in_line_com lexbuf;
                   virt_lnum := lexbuf.Lexing.lex_curr_p.pos_lnum
    |  "hidden" -> in_line_com lexbuf
    |         _ -> fail "Invalid line indicator." lexbuf
  }
| _ { fail "Line indicator expected." lexbuf }

and end_indicator = parse
  blank* newline { copy lexbuf; handle_nl lexbuf }
| blank* "//"    { copy lexbuf; in_line_com lexbuf }
| blank+ '"'     { copy lexbuf;
                   let _ = handle_err (in_norm_str 0 []) lexbuf
                   in opt_line_com lexbuf }
| [^' ' '\t']    { fail "Blank expected." lexbuf }
| blank          { halt "Line comment or string expected." lexbuf }

and opt_line_com = parse
  newline { copy lexbuf; handle_nl lexbuf }
| eof     { copy lexbuf }
| blank+  { copy lexbuf; opt_line_com lexbuf }
| "//"    { copy lexbuf; in_line_com lexbuf }
| _       { fail "Line comment or newline expected." lexbuf }


(* Comments *)

and in_line_com = parse
  newline { copy lexbuf; handle_nl lexbuf }
| eof     { copy lexbuf }
| _       { copy lexbuf; in_line_com lexbuf }
  
and in_block_com = parse
  newline { copy lexbuf; handle_nl lexbuf; in_block_com lexbuf }
| "*/"    { copy lexbuf }
| eof     { raise (Local_err "Unterminated comment.") }
| _       { copy lexbuf; in_block_com lexbuf }

(* Strings *)

and in_norm_str len acc = parse
  newline { raise (Local_err "Newline invalid in string.") }
| '"'     { copy lexbuf;
            Lexis.String(Norm, mk_str len acc), token }
| eof     { raise (Local_err "Unterminated string.") }
| _ as c  { copy lexbuf; in_norm_str (len+1) (c::acc) lexbuf }

and in_verb_str len acc = parse
  newline as nl
          { copy lexbuf; handle_nl lexbuf;
            in_verb_str (len+1) (explode nl acc) lexbuf }
| '"'     { copy lexbuf;
            Lexis.String(Verb, mk_str len acc), token }
| eof     { raise (Local_err "Unterminated string.") }
| _ as c  { copy lexbuf; in_verb_str (len+1) (c::acc) lexbuf }

{

(* The call [get_token buffer] returns a token, as expected by
   [menhir]. The parser cannot use directly the lexer [token] because
   it is streamlined. *)

let get_token : Lexing.lexbuf -> Lexis.token =
  let strlex = ref token
in fun buffer -> let t, strlex' = !strlex buffer
                 in strlex := strlex'; t

(* Standalone lexer for debugging purposes. See module [Topcopy]. *)

type filename = string

let trace ~(input: filename) ~(output: filename) =
  match open_in input, open_out output with
    cin, cout ->
      let buffer = Lexing.from_channel cin in
      let rec iter lexer =
        match lexer buffer with
          Lexis.EOF, _ -> close_in cin; close_out cout
        |    t, lexer' -> begin
                            output_string cout (string_of_token t);
                            output_string cout "\n";
                            flush cout;
                            iter lexer'
                          end
        | exception Error.Lexer diag -> 
            close_in cin; close_out cout; Error.print "Lexical" diag
      in iter token
  | exception Sys_error msg -> prerr_endline msg
}
