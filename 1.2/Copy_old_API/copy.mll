{
(* Lexer specification of C# 1.2 for ocamllex 
   based on the old signature of module [Lexing] *)

(* Positions *)

let lex_curr_p : Error.position ref =
  ref Error.{ pos_fname = ""; pos_lnum = 1; pos_cnum = 0; pos_bol = 0 }

let lexeme_length buffer =
  Lexing.(lexeme_end buffer - lexeme_start buffer)

(* [update buffer] not to be called on a new line *)

let update buffer : unit =
  lex_curr_p :=
    Error.{ !lex_curr_p with
      pos_cnum = !lex_curr_p.pos_cnum + lexeme_length buffer }

(* Virtual line number (according to #line) and end of lines *)

let virt_lnum = ref 1

let handle_nl buffer =
  let nl = Lexing.lexeme buffer
in begin
     incr virt_lnum;
     lex_curr_p := Error.{ !lex_curr_p with
       pos_lnum = !lex_curr_p.pos_lnum + 1;
       pos_cnum = !lex_curr_p.pos_cnum + String.length nl;
       pos_bol  = !lex_curr_p.pos_cnum + 1 }
   end

(* Copying the matched prefix of the input buffer to an output channel
*)

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

(* Error handling *)

let halt msg =
  raise (Error.(Lexer (msg, (!lex_curr_p, !lex_curr_p), !virt_lnum)))

let stop msg buffer =
  let lexeme_start_p = !lex_curr_p
in update buffer;
   raise (Error.(Lexer (msg, (lexeme_start_p, !lex_curr_p), !virt_lnum)))

exception Local_err of Error.message

let handle_err scan buffer =
  try scan buffer with Local_err msg -> stop msg buffer

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
| blank+       { update lexbuf; copy lexbuf; token lexbuf }

| "abstract"   { update lexbuf; copy lexbuf; Lexis.ABSTRACT,   token }
| "as"         { update lexbuf; copy lexbuf; Lexis.AS,         token }
| "base"       { update lexbuf; copy lexbuf; Lexis.BASE,       token }
| "bool"       { update lexbuf; copy lexbuf; Lexis.BOOL,       token }
| "break"      { update lexbuf; copy lexbuf; Lexis.BREAK,      token }
| "byte"       { update lexbuf; copy lexbuf; Lexis.BYTE,       token }
| "case"       { update lexbuf; copy lexbuf; Lexis.CASE,       token }
| "catch"      { update lexbuf; copy lexbuf; Lexis.CATCH,      token }
| "char"       { update lexbuf; copy lexbuf; Lexis.CHAR,       token }
| "checked"    { update lexbuf; copy lexbuf; Lexis.CHECKED,    token }
| "class"      { update lexbuf; copy lexbuf; Lexis.CLASS,      token }
| "const"      { update lexbuf; copy lexbuf; Lexis.CONST,      token }
| "continue"   { update lexbuf; copy lexbuf; Lexis.CONTINUE,   token }
| "decimal"    { update lexbuf; copy lexbuf; Lexis.DECIMAL,    token }
| "default"    { update lexbuf; copy lexbuf; Lexis.DEFAULT,    token }
| "delegate"   { update lexbuf; copy lexbuf; Lexis.DELEGATE,   token }
| "do"         { update lexbuf; copy lexbuf; Lexis.DO,         token }
| "double"     { update lexbuf; copy lexbuf; Lexis.DOUBLE,     token }
| "else"       { update lexbuf; copy lexbuf; Lexis.ELSE,       token }
| "enum"       { update lexbuf; copy lexbuf; Lexis.ENUM,       token }
| "event"      { update lexbuf; copy lexbuf; Lexis.EVENT,      token }
| "explicit"   { update lexbuf; copy lexbuf; Lexis.EXPLICIT,   token }
| "extern"     { update lexbuf; copy lexbuf; Lexis.EXTERN,     token }
| "false"      { update lexbuf; copy lexbuf; Lexis.FALSE,      token }
| "finally"    { update lexbuf; copy lexbuf; Lexis.FINALLY,    token }
| "fixed"      { update lexbuf; copy lexbuf; Lexis.FIXED,      token }
| "float"      { update lexbuf; copy lexbuf; Lexis.FLOAT,      token }
| "for"        { update lexbuf; copy lexbuf; Lexis.FOR,        token }
| "foreach"    { update lexbuf; copy lexbuf; Lexis.FOREACH,    token }
| "goto"       { update lexbuf; copy lexbuf; Lexis.GOTO,       token }
| "if"         { update lexbuf; copy lexbuf; Lexis.IF,         token }
| "implicit"   { update lexbuf; copy lexbuf; Lexis.IMPLICIT,   token }
| "in"         { update lexbuf; copy lexbuf; Lexis.IN,         token }
| "int"        { update lexbuf; copy lexbuf; Lexis.INT,        token }
| "interface"  { update lexbuf; copy lexbuf; Lexis.INTERFACE,  token }
| "internal"   { update lexbuf; copy lexbuf; Lexis.INTERNAL,   token }
| "is"         { update lexbuf; copy lexbuf; Lexis.IS,         token }
| "lock"       { update lexbuf; copy lexbuf; Lexis.LOCK,       token }
| "long"       { update lexbuf; copy lexbuf; Lexis.LONG,       token }
| "namespace"  { update lexbuf; copy lexbuf; Lexis.NAMESPACE,  token }
| "new"        { update lexbuf; copy lexbuf; Lexis.NEW,        token }
| "null"       { update lexbuf; copy lexbuf; Lexis.NULL,       token }
| "object"     { update lexbuf; copy lexbuf; Lexis.OBJECT,     token }
| "operator"   { update lexbuf; copy lexbuf; Lexis.OPERATOR,   token }
| "out"        { update lexbuf; copy lexbuf; Lexis.OUT,        token }
| "override"   { update lexbuf; copy lexbuf; Lexis.OVERRIDE,   token }
| "params"     { update lexbuf; copy lexbuf; Lexis.PARAMS,     token }
| "private"    { update lexbuf; copy lexbuf; Lexis.PRIVATE,    token }
| "protected"  { update lexbuf; copy lexbuf; Lexis.PROTECTED,  token }
| "public"     { update lexbuf; copy lexbuf; Lexis.PUBLIC,     token }
| "readonly"   { update lexbuf; copy lexbuf; Lexis.READONLY,   token }
| "ref"        { update lexbuf; copy lexbuf; Lexis.REF,        token }
| "return"     { update lexbuf; copy lexbuf; Lexis.RETURN,     token }
| "sbyte"      { update lexbuf; copy lexbuf; Lexis.SBYTE,      token }
| "sealed"     { update lexbuf; copy lexbuf; Lexis.SEALED,     token }
| "short"      { update lexbuf; copy lexbuf; Lexis.SHORT,      token }
| "sizeof"     { update lexbuf; copy lexbuf; Lexis.SIZEOF,     token }
| "stackalloc" { update lexbuf; copy lexbuf; Lexis.STACKALLOC, token }
| "static"     { update lexbuf; copy lexbuf; Lexis.STATIC,     token }
| "string"     { update lexbuf; copy lexbuf; Lexis.STRING,     token }
| "struct"     { update lexbuf; copy lexbuf; Lexis.STRUCT,     token }
| "switch"     { update lexbuf; copy lexbuf; Lexis.SWITCH,     token }
| "this"       { update lexbuf; copy lexbuf; Lexis.THIS,       token }
| "throw"      { update lexbuf; copy lexbuf; Lexis.THROW,      token }
| "true"       { update lexbuf; copy lexbuf; Lexis.TRUE,       token }
| "try"        { update lexbuf; copy lexbuf; Lexis.TRY,        token }
| "typeof"     { update lexbuf; copy lexbuf; Lexis.TYPEOF,     token }
| "uint"       { update lexbuf; copy lexbuf; Lexis.UINT,       token }
| "ulong"      { update lexbuf; copy lexbuf; Lexis.ULONG,      token }
| "unchecked"  { update lexbuf; copy lexbuf; Lexis.UNCHECKED,  token }
| "unsafe"     { update lexbuf; copy lexbuf; Lexis.UNSAFE,     token }
| "ushort"     { update lexbuf; copy lexbuf; Lexis.USHORT,     token }
| "using"      { update lexbuf; copy lexbuf; Lexis.USING,      token }
| "virtual"    { update lexbuf; copy lexbuf; Lexis.VIRTUAL,    token }
| "void"       { update lexbuf; copy lexbuf; Lexis.VOID,       token }
| "volatile"   { update lexbuf; copy lexbuf; Lexis.VOLATILE,   token }
| "while"      { update lexbuf; copy lexbuf; Lexis.WHILE,      token }

| '{'          { update lexbuf; copy lexbuf; Lexis.LBRACE,     token }
| '}'          { update lexbuf; copy lexbuf; Lexis.RBRACE,     token }
| '['          { update lexbuf; copy lexbuf; Lexis.LBRACKET,   token }
| ']'          { update lexbuf; copy lexbuf; Lexis.RBRACKET,   token }
| '('          { update lexbuf; copy lexbuf; after_LPAR token lexbuf  }
| ')'          { update lexbuf; copy lexbuf; Lexis.RPAR,       token }
| '.'          { update lexbuf; copy lexbuf; Lexis.DOT,        token }
| ','          { update lexbuf; copy lexbuf; Lexis.COMMA,      token }
| ':'          { update lexbuf; copy lexbuf; Lexis.COLON,      token }
| ';'          { update lexbuf; copy lexbuf; Lexis.SEMI,       token }

| '+'          { update lexbuf; copy lexbuf; Lexis.PLUS,       token }
| '-'          { update lexbuf; copy lexbuf; Lexis.MINUS,      token }
| '*'          { update lexbuf; copy lexbuf; Lexis.TIMES,      token }
| '/'          { update lexbuf; copy lexbuf; Lexis.SLASH,      token }
| '%'          { update lexbuf; copy lexbuf; Lexis.PERCENT,    token }
| '&'          { update lexbuf; copy lexbuf; Lexis.AMPER,      token }
| '|'          { update lexbuf; copy lexbuf; Lexis.MID,        token }
| '^'          { update lexbuf; copy lexbuf; Lexis.CIRCUM,     token }
| '!'          { update lexbuf; copy lexbuf; Lexis.BANG,       token }
| '~'          { update lexbuf; copy lexbuf; Lexis.TILDE,      token }

| '='          { update lexbuf; copy lexbuf; Lexis.ASSIGN,     token }
| '<'          { update lexbuf; copy lexbuf; Lexis.LT,         token }
| '>'          { update lexbuf; copy lexbuf; Lexis.GT,         token }
| '?'          { update lexbuf; copy lexbuf; Lexis.QMARK,      token }
| "++"         { update lexbuf; copy lexbuf; Lexis.DPLUS,      token }
| "--"         { update lexbuf; copy lexbuf; Lexis.DMINUS,     token }
| "&&"         { update lexbuf; copy lexbuf; Lexis.DAMPER,     token }
| "||"         { update lexbuf; copy lexbuf; Lexis.DMID,       token }
| "<<"         { update lexbuf; copy lexbuf; Lexis.LSHIFT,     token }
| ">>"         { update lexbuf; copy lexbuf; Lexis.RSHIFT,     token }

| "=="         { update lexbuf; copy lexbuf; Lexis.EQ,         token }
| "!="         { update lexbuf; copy lexbuf; Lexis.NE,         token }
| "+="         { update lexbuf; copy lexbuf; Lexis.PLUSEQ,     token }
| "-="         { update lexbuf; copy lexbuf; Lexis.MINUSEQ,    token }
| "*="         { update lexbuf; copy lexbuf; Lexis.TIMESEQ,    token }
| "/="         { update lexbuf; copy lexbuf; Lexis.DIVEQ,      token }
| "%="         { update lexbuf; copy lexbuf; Lexis.PEREQ,      token }
| "&="         { update lexbuf; copy lexbuf; Lexis.AMPEREQ,    token }
| "|="         { update lexbuf; copy lexbuf; Lexis.MIDEQ,      token }
| "^="         { update lexbuf; copy lexbuf; Lexis.CIREQ,      token }

| "<<="        { update lexbuf; copy lexbuf; Lexis.LSHIFTEQ,   token }
| "<="         { update lexbuf; copy lexbuf; Lexis.LEQ,        token }
| ">>="        { update lexbuf; copy lexbuf; Lexis.RSHIFTEQ,   token }
| ">="         { update lexbuf; copy lexbuf; Lexis.GEQ,        token }

| "->"         { update lexbuf; copy lexbuf; Lexis.ARROW,      token }

| integer as s     { update lexbuf; copy lexbuf; Lexis.Int s,         token }
| real as s        { update lexbuf; copy lexbuf; Lexis.Float s,       token }
| ident as s       { update lexbuf; copy lexbuf; Lexis.Ident(Norm,s), token }
| '@' (ident as s) { update lexbuf; copy lexbuf; Lexis.Ident(Verb,s), token }
| char as s        { update lexbuf; copy lexbuf; Lexis.Char s,        token }

| '"'   { update lexbuf; copy lexbuf;
          handle_err (in_norm_str 0 []) lexbuf }
| "@\"" { update lexbuf; copy lexbuf;
          handle_err (in_verb_str 0 []) lexbuf }

| "//"  { update lexbuf; copy lexbuf;
          handle_err in_line_com lexbuf; token lexbuf  }
| "/*"  { update lexbuf; copy lexbuf;
          handle_err in_block_com lexbuf; token lexbuf }

| eof { update lexbuf; copy lexbuf; Lexis.EOF, token }

| '#' blank* "line" blank* (newline | eof) {
    update lexbuf; halt "Line indicator expected." }
| '#' blank* "line" blank+ {
    update lexbuf; copy lexbuf;
    line_indicator lexbuf; token lexbuf }

| _ { update lexbuf; copy lexbuf; token lexbuf }

(* Line indicator (#line) *)

and line_indicator = parse
  decimal as n {
    update lexbuf; copy lexbuf;
    end_indicator lexbuf;
    virt_lnum := int_of_string n
  }
| ident as id {
    match id with
      "default" -> update lexbuf; copy lexbuf; in_line_com lexbuf;
                   virt_lnum := !lex_curr_p.Error.pos_lnum
    |  "hidden" -> update lexbuf; copy lexbuf; in_line_com lexbuf
    |         _ -> stop ("Invalid line indicator `" ^ id ^ "'.") lexbuf
  }
| _ { stop "Line indicator expected." lexbuf }

and end_indicator = parse
  blank* newline { update lexbuf; copy lexbuf; handle_nl lexbuf }
| blank* "//"    { update lexbuf; copy lexbuf; in_line_com lexbuf }
| blank+ '"'     { update lexbuf; copy lexbuf;
                   let _ = handle_err (in_norm_str 0 []) lexbuf
                   in opt_line_com lexbuf }
| [^' ' '\t']    { stop "Blank expected." lexbuf }
| blank          { update lexbuf;
                   halt "Line comment or string expected." }

and opt_line_com = parse
  newline { update lexbuf; copy lexbuf; handle_nl lexbuf }
| eof     { update lexbuf; copy lexbuf }
| blank+  { update lexbuf; copy lexbuf; opt_line_com lexbuf }
| "//"    { update lexbuf; copy lexbuf; in_line_com lexbuf }
| _       { stop "Line comment or newline expected." lexbuf }

(* Comments *)

and in_line_com = parse
  newline { update lexbuf; copy lexbuf; handle_nl lexbuf }
| eof     { update lexbuf; copy lexbuf }
| _       { update lexbuf; copy lexbuf; in_line_com lexbuf }
  
and in_block_com = parse
  newline { update lexbuf; copy lexbuf;
            handle_nl lexbuf; in_block_com lexbuf }
| "*/"    { update lexbuf; copy lexbuf }
| eof     { raise (Local_err "Unterminated comment.") }
| _       { update lexbuf; copy lexbuf; in_block_com lexbuf }

(* Strings *)

and in_norm_str len acc = parse
  newline { raise (Local_err "Newline invalid in string.") }
| '"'     { update lexbuf; copy lexbuf;
            Lexis.String(Norm, mk_str len acc), token }
| eof     { raise (Local_err "Unterminated string.") }
| _ as c  { update lexbuf; copy lexbuf;
            in_norm_str (len+1) (c::acc) lexbuf }

and in_verb_str len acc = parse
  newline as nl
          { update lexbuf; copy lexbuf; handle_nl lexbuf;
            in_verb_str (len+1) (explode nl acc) lexbuf }
| '"'     { update lexbuf; copy lexbuf;
            Lexis.String(Verb, mk_str len acc), token }
| eof     { raise (Local_err "Unterminated string.") }
| _ as c  { update lexbuf; copy lexbuf;
            in_verb_str (len+1) (c::acc) lexbuf }

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
      let () =
        lex_curr_p := Error.{ !lex_curr_p with pos_fname = input } in
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
            close_in cin; close_out cout; Error.print "lexical" diag
      in iter token
  | exception Sys_error msg -> prerr_endline msg
}
