{
(* Lexer for C# 1.2, to be processed by [ocamllex] *)

(* String processing *)

let mk_str (len:int) (p:char list) : string =
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

(* Only preprocessing ([Alone]) or not *)

type status = Alone | Coop

(* Copying the current lexeme to [stdout] or resuming with a given
   scanner *)

let copy scanner buffer = function
  Alone -> print_string (Lexing.lexeme buffer)
|  Coop -> scanner buffer

(* Error handling *)

let stop msg seg = raise (Error.Lexer (msg, seg, !virt_lnum))
let fail msg buffer = stop msg (Error.mk_seg buffer)

let halt msg buffer =
  let pos = buffer.Lexing.lex_curr_p
in raise (Error.(Lexer (msg, (pos, pos), !virt_lnum)))

exception Local_err of Error.message

let handle_err scan buffer =
  try scan buffer with Local_err msg -> fail msg buffer

(* Scanning modes *)

type mode = Copy | Skip

(* Trace of conditionals *)

type cond = If of mode | Elif of mode | Else | Region
type trace = cond list

let rec reduce_cond seg = function
              [] -> stop "Dangling #endif." seg
| If mode::trace -> trace, mode
|  Region::trace -> stop "Invalid scoping of #region" seg
|       _::trace -> reduce_cond seg trace

let reduce_reg seg = function
             [] -> stop "Dangling #endregion." seg
| Region::trace -> trace
|             _ -> stop "Invalid scoping of #endregion" seg

let extend seg cond trace =
  match cond, trace with
    If _, Elif _::_ ->
      stop "Directive #if cannot follow #elif." seg
  | Else,   Else::_ ->
      stop "Directive #else cannot follow #else." seg
  | Else,        [] ->
      stop "Dangling #else." seg
  | Elif _, Else::_ ->
      stop "Directive #elif cannot follow #else." seg
  | Elif _,      [] ->
      stop "Dangling #elif." seg
  |               _ -> cond::trace

let rec last_mode = function
                        [] -> assert false
| (If mode | Elif mode)::_ -> mode
|                 _::trace -> last_mode trace

(* Line offsets *)

type offset = Prefix of int | Inline

let expand status = function
  Prefix 0 | Inline -> ()
| Prefix n -> if status = Alone
              then print_string (String.make n ' ')

(* Directives *)

let directives = ["if"; "else"; "elif"; "endif"; "define"; "undef";
                  "error"; "warning"; "line"; "region"; "endregion"]

(* Environments and preprocessor expressions *)

module Env = Set.Make(String)

let rec eval env =
  let open Etree
in function Or (e1,e2) -> eval env e1 || eval env e2
         | And (e1,e2) -> eval env e1 && eval env e2
         |  Eq (e1,e2) -> eval env e1 = eval env e2
         | Neq (e1,e2) -> eval env e1 != eval env e2
         |       Not e -> not (eval env e)
         |        True -> true
         |       False -> false
         |    Ident id -> Env.mem id env

(* Concrete syntax of tokens. See module [Parser]. *)

let string_of_token = 
  let open Parser in function
  (* Keywords *)

    ABSTRACT -> "abstract"
|         AS -> "as"
|       BASE -> "base"
|       BOOL -> "bool"
|      BREAK -> "break"
|       BYTE -> "byte"
|       CASE -> "case"
|      CATCH -> "catch"
|       CHAR -> "char"
|    CHECKED -> "checked"
|      CLASS -> "class"
|      CONST -> "const"
|   CONTINUE -> "continue"
|    DECIMAL -> "decimal"
|    DEFAULT -> "default"
|   DELEGATE -> "delegate"
|         DO -> "do"
|     DOUBLE -> "double"
|       ELSE -> "else"
|       ENUM -> "enum"
|      EVENT -> "event"
|   EXPLICIT -> "explicit"
|     EXTERN -> "extern"
|      FALSE -> "false"
|    FINALLY -> "finally"
|      FIXED -> "fixed"
|      FLOAT -> "float"
|        FOR -> "for"
|    FOREACH -> "foreach"
|       GOTO -> "goto"
|         IF -> "if"
|   IMPLICIT -> "implicit"
|         IN -> "in"
|        INT -> "int"
|  INTERFACE -> "interface"
|   INTERNAL -> "internal"
|         IS -> "is"
|       LOCK -> "lock"
|       LONG -> "long"
|  NAMESPACE -> "namespace"
|        NEW -> "new"
|       NULL -> "null"
|     OBJECT -> "object"
|   OPERATOR -> "operator"
|        OUT -> "out"
|   OVERRIDE -> "override"
|     PARAMS -> "params"
|    PRIVATE -> "private"
|  PROTECTED -> "protected"
|     PUBLIC -> "public"
|   READONLY -> "readonly"
|        REF -> "ref"
|     RETURN -> "return"
|      SBYTE -> "sbyte"
|     SEALED -> "sealed"
|      SHORT -> "short"
|     SIZEOF -> "sizeof"
| STACKALLOC -> "stackalloc"
|     STATIC -> "static"
|     STRING -> "string"
|     STRUCT -> "struct"
|     SWITCH -> "switch"
|       THIS -> "this"
|      THROW -> "throw"
|       TRUE -> "true"
|        TRY -> "try"
|     TYPEOF -> "typeof"
|       UINT -> "uint"
|      ULONG -> "ulong"
|  UNCHECKED -> "unchecked"
|     UNSAFE -> "unsafe"
|     USHORT -> "ushort"
|      USING -> "using"
|    VIRTUAL -> "virtual"
|       VOID -> "void"
|   VOLATILE -> "volatile"
|      WHILE -> "while"

(* Symbols and operators *)

|     LBRACE -> "{"
|     RBRACE -> "}"
|   LBRACKET -> "["
|   RBRACKET -> "]"
|       LPAR -> "("
|       RPAR -> ")"
|        DOT -> "."
|      COMMA -> ","
|      COLON -> ":"
|       SEMI -> ";"
|       PLUS -> "+"
|      MINUS -> "-"
|      TIMES -> "*"
|      SLASH -> "/"
|    PERCENT -> "%"
|      AMPER -> "&"
|        MID -> "|"
|     CIRCUM -> "^"
|       BANG -> "!"
|      TILDE -> "~"
|     ASSIGN -> "="
|         LT -> "<"
|         GT -> ">"
|      QMARK -> "?"
|      DPLUS -> "++"
|     DMINUS -> "--"
|     DAMPER -> "&&"
|       DMID -> "||"
|     LSHIFT -> "<<"
|     RSHIFT -> ">>"
|         EQ -> "=="
|         NE -> "!="
|        LEQ -> "<="
|        GEQ -> ">="
|     PLUSEQ -> "+="
|    MINUSEQ -> "-="
|    TIMESEQ -> "*="
|      DIVEQ -> "/="
|      PEREQ -> "%="
|    AMPEREQ -> "&="

|      MIDEQ -> "|="
|      CIREQ -> "^="
|   LSHIFTEQ -> "<<="
|   RSHIFTEQ -> ">>="
|      ARROW -> "->"

(* Literals *)

| Ident (Lexis.Norm,s)  -> "Ident(" ^ s ^ ")"
| Ident (Lexis.Verb,s)  -> "Ident(@" ^ s ^ ")"
| Int s                 -> "Int(" ^ s ^ ")"
| Float s               -> "Float(" ^ s ^ ")" 
| Char s                -> "Char(" ^ s ^ ")"
| String (Lexis.Norm,s) -> "String(\"" ^ s ^ "\")"
| String (Lexis.Verb,s) -> "String(@\"" ^ s ^ "\")"

(* Virtual tokens *)

| EOF  -> "EOF" (* End of File *)
| CAST -> "CAST"

(* Keywords *)

let kwd_lst =
  let open Parser in [
  "abstract",     ABSTRACT;
  "as",                 AS;
  "base",             BASE;
  "bool",             BOOL;
  "break",           BREAK;
  "byte",             BYTE;
  "case",             CASE;
  "catch",           CATCH;
  "char",             CHAR;
  "checked",       CHECKED;
  "class",           CLASS;
  "const",           CONST;
  "continue",     CONTINUE;
  "decimal",       DECIMAL;
  "default",       DEFAULT;
  "delegate",     DELEGATE;
  "do",                 DO;
  "double",         DOUBLE;
  "else",             ELSE;
  "enum",             ENUM;
  "event",           EVENT;
  "explicit",     EXPLICIT;
  "extern",         EXTERN;
  "false",           FALSE;
  "finally",       FINALLY;
  "fixed",           FIXED;
  "float",           FLOAT;
  "for",               FOR;
  "foreach",       FOREACH;
  "goto",             GOTO;
  "if",                 IF;
  "implicit",     IMPLICIT;
  "in",                 IN;
  "int",               INT;
  "interface",   INTERFACE;
  "internal",     INTERNAL;
  "is",                 IS;
  "lock",             LOCK;
  "long",             LONG;
  "namespace",   NAMESPACE;
  "new",               NEW;
  "null",             NULL;
  "object",         OBJECT;
  "operator",     OPERATOR;
  "out",               OUT;
  "override",     OVERRIDE;
  "params",         PARAMS;
  "private",       PRIVATE;
  "protected",   PROTECTED;
  "public",         PUBLIC;
  "readonly",     READONLY;
  "ref",               REF;
  "return",         RETURN;
  "sbyte",           SBYTE;
  "sealed",         SEALED;
  "short",           SHORT;
  "sizeof",         SIZEOF;
  "stackalloc", STACKALLOC;
  "static",         STATIC;
  "string",         STRING;
  "struct",         STRUCT;
  "switch",         SWITCH;
  "this",             THIS;
  "throw",           THROW;
  "true",             TRUE;
  "try",               TRY;
  "typeof",         TYPEOF;
  "uint",             UINT;
  "ulong",           ULONG;
  "unchecked",   UNCHECKED;
  "unsafe",         UNSAFE;
  "ushort",         USHORT;
  "using",           USING;
  "virtual",       VIRTUAL;
  "void",             VOID;
  "volatile",     VOLATILE;
  "while",           WHILE]

module Kwd = Map.Make(String)

let kwd_map =
  List.fold_left (fun set (key,token) -> Kwd.add key token set)
                 Kwd.empty
                 kwd_lst

(* Concrete syntax of preprocessor expression tokens.
   See module [Eparser] for type definition. *)

let string_of_ppe_token =
  let open Eparser
in function True -> "true"
      |    False -> "false"
      | Ident id -> id
      |       OR -> "||"
      |      AND -> "&&"
      |       EQ -> "=="
      |      NEQ -> "!="
      |      NOT -> "!"
      |     LPAR -> "("
      |     RPAR -> ")"
      |      EOL -> "EOL"

(* Deterministic finite automaton to recognise some C# casts and emit
   the virtual token CAST in prefix position accordingly, in order to
   solve a conflict in the parser between expressions (multiplication
   and dereferencing) and casts. *)

type streamlined = Lexing.lexbuf -> Parser.token * 'a as 'a

let rec pad (lexer: streamlined) = function
             [] -> assert false
|       [token] -> token, lexer
| token::tokens -> token, fun _ -> pad lexer tokens 

let mk_cast tokens lexer = pad lexer (Parser.CAST :: List.rev tokens)

let tokenise tokens lexer = pad lexer (List.rev tokens)

let chk_cast tokens lexer buffer =
  let open Parser in
  let token, lexer' = lexer buffer in
  let tokens' = token::tokens
in match token with
     LPAR | CAST | Ident _ -> mk_cast tokens' lexer'
   |                     _ -> tokenise tokens' lexer'

let rec after_LPAR (lexer: streamlined) (buffer: Lexing.lexbuf) =
  let open Parser in
  let token, lexer' = lexer buffer in
  let tokens' = [token;LPAR]
in match token with
     SBYTE | BYTE | SHORT | USHORT | INT | UINT | LONG 
   | ULONG | CHAR | FLOAT | DOUBLE | DECIMAL | BOOL
   | OBJECT | STRING -> after_kwd tokens' lexer' buffer
   |         Ident _ -> after_id tokens' lexer' buffer
   |               _ -> tokenise tokens' lexer'

and after_id tokens lexer buffer =
  let open Parser in
  let token, lexer' = lexer buffer in
  let tokens' = token::tokens
in match token with
         RPAR -> chk_cast tokens' lexer' buffer
   |    TIMES -> after_times tokens' lexer' buffer
   | LBRACKET -> after_fst_lbr tokens' lexer' buffer
   |      DOT -> after_dot tokens' lexer' buffer
   |        _ -> tokenise tokens' lexer'

and after_kwd tokens lexer buffer =
  let open Parser in
  let token, lexer' = lexer buffer in
  let tokens' = token::tokens
in match token with
         RPAR -> mk_cast tokens' lexer'
   |    TIMES -> after_times tokens' lexer' buffer
   | LBRACKET -> after_fst_lbr tokens' lexer' buffer
   |        _ -> tokenise tokens' lexer'

and after_dot tokens lexer buffer =
  let open Parser in
  let token, lexer' = lexer buffer in
  let tokens' = token::tokens
in match token with
     Ident _ -> after_id tokens' lexer' buffer
   |       _ -> tokenise tokens' lexer'

and after_times tokens lexer buffer =
  let open Parser in
  let token, lexer' = lexer buffer in
  let tokens' = token::tokens
in match token with
        TIMES -> after_times tokens' lexer' buffer
   |     RPAR -> mk_cast tokens' lexer'
   | LBRACKET -> after_fst_lbr tokens' lexer' buffer
   |        _ -> tokenise tokens' lexer'

and after_fst_lbr tokens lexer buffer =
  let open Parser in
  let token, lexer' = lexer buffer in
  let tokens' = token::tokens
in match token with
        COMMA -> after_fst_lbr tokens' lexer' buffer
   | RBRACKET -> after_more_lbr tokens' lexer' buffer
   |        _ -> tokenise tokens' lexer'

and after_more_lbr tokens lexer buffer =
  let open Parser in
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

let repeat t n (lexer: Lexing.lexbuf -> Parser.token * 'a as 'a) =
  assert (n > 0);
  let rec make n : Parser.token * 'a =
    t, if n = 1 then lexer else fun _ -> make (n-1)
in make n

let rec rm_cast = function
                   [] -> []
| Parser.CAST::tokens -> rm_cast tokens
|       token::tokens -> token :: rm_cast tokens

}

(* Regular expressions for literals *)

(* White space *)

let newline = '\n' | '\r' | "\r\n"
let blank = [' ' '\t']

(* Integers *)

let int_suf = ['U' 'u' 'L' 'l']
            | "UL" | "Ul" | "uL" | "ul" | "LU" | "Lu" | "lU" | "lu"
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

(* Directives *)

let directive = '#' (blank* as space) (ident as id)

(* Rules for the preprocessor expressions *)

rule ppe_token = parse
  blank+      { ppe_token lexbuf  }
| newline     { handle_nl lexbuf; Eparser.EOL }
| eof         { Eparser.EOL       }
| "true"      { Eparser.True      }
| "false"     { Eparser.False     }
| ident as id { Eparser.Ident id  }
| '('         { Eparser.LPAR      }
| ')'         { Eparser.RPAR      }
| "||"        { Eparser.OR        }
| "&&"        { Eparser.AND       }
| "=="        { Eparser.EQ        }
| "!="        { Eparser.NEQ       }
| "!"         { Eparser.NOT       }
| "//"        { ppe_in_line_com lexbuf }
| _ as c      { let code = Char.code c in
                let msg = "Invalid character " ^ String.make 1 c
                          ^ " (" ^ string_of_int code ^ ")."
                in fail msg lexbuf }

and ppe_in_line_com = parse
  newline { handle_nl lexbuf; Eparser.EOL }
| eof     { Eparser.EOL }
| _       { ppe_in_line_com lexbuf }


(* Preprocessing directives *)

and pp_scan status env mode offset trace = parse
  newline { handle_nl lexbuf; copy ignore lexbuf status;
            pp_scan status env mode (Prefix 0) trace lexbuf }
| blank { match offset with
            Prefix n -> pp_scan status env mode (Prefix (n+1)) trace lexbuf
          |   Inline -> copy ignore lexbuf status;
                        pp_scan status env mode Inline trace lexbuf }
| directive {
   let expr env buffer =
     let tree = Eparser.pp_expression ppe_token buffer
     in if eval env tree then Copy else Skip
   in if not (List.mem id directives)
      then fail "Invalid preprocessing directive." lexbuf
      else if offset = Inline
           then fail "Directive invalid inside line." lexbuf
           else let seg = Error.mk_seg lexbuf in 
      match id with
        "if" ->
          let mode' = expr env lexbuf in
          let new_mode = if mode = Copy then mode' else Skip in
          let trace' = extend seg (If mode) trace
          in pp_scan status env new_mode (Prefix 0) trace' lexbuf
      | "else" ->
          let () = pp_newline status lexbuf in
          let new_mode =
            if mode = Copy then Skip else last_mode trace in
          let trace' = extend seg Else trace
          in pp_scan status env new_mode (Prefix 0) trace' lexbuf
      | "elif" ->
          let mode' = expr env lexbuf in
          let trace', new_mode =
            match mode with
              Copy -> extend seg (Elif Skip) trace, Skip
            | Skip -> let old_mode = last_mode trace
                      in extend seg (Elif old_mode) trace,
                         if old_mode = Copy then mode' else Skip
          in pp_scan status env new_mode (Prefix 0) trace' lexbuf
      | "endif" ->
          let () = pp_newline status lexbuf in
          let trace', new_mode = reduce_cond seg trace
          in pp_scan status env new_mode (Prefix 0) trace' lexbuf
      | "define" ->
          let id, seg = pp_ident status env lexbuf
          in if id="true" || id="false"
             then let msg = "Symbol \"" ^ id ^ "\" cannot be defined."
                  in stop msg seg
             else if Env.mem id env
                  then let msg = "Symbol \"" ^ id
                                 ^ "\" was already defined."
                       in stop msg seg
                  else pp_scan status (Env.add id env) mode (Prefix 0)
                               trace lexbuf
      | "undef" ->
          let id, _ = pp_ident status env lexbuf
          in pp_scan status (Env.remove id env) mode (Prefix 0) trace lexbuf
      | "error" ->
          stop (message [] lexbuf) seg
      | "warning" ->
          let start_p, end_p = seg in
          let msg = message [] lexbuf
          in prerr_endline 
               ("Warning at line " ^ string_of_int start_p.pos_lnum 
               ^ ", char "
               ^ string_of_int (start_p.pos_cnum - start_p.pos_bol)
               ^ "--" ^ string_of_int (end_p.pos_cnum - end_p.pos_bol)
               ^ ":\n" ^ msg);
             pp_scan status env mode (Prefix 0) trace lexbuf
      | "region" ->
          let msg = message [] lexbuf
          in expand status offset;
             if status = Alone
             then print_endline ("#" ^ space ^ "region" ^ msg);
             pp_scan status env mode (Prefix 0) (Region::trace) lexbuf
      | "endregion" ->
          let msg = message [] lexbuf
          in expand status offset;
             if status = Alone
             then print_endline ("#" ^ space ^ "endregion" ^ msg);
             pp_scan status env mode (Prefix 0) 
                     (reduce_reg seg trace) lexbuf
      | "line" ->
          expand status offset;
          if status = Alone
          then print_string ("#" ^ space ^ "line");
          pp_line_ind status lexbuf;
          pp_scan status env mode (Prefix 0) trace lexbuf
      | s -> assert false 
    }
| eof   { match trace with
            [] -> expand status offset;
                  if status = Alone then flush stdout;
                  (env, trace) 
          |  _ -> fail "Missing #endif." lexbuf }
| '"'   { if mode = Copy then begin
             expand status offset; copy ignore lexbuf status;
             handle_err (pp_in_norm_str status) lexbuf
          end;
          pp_scan status env mode Inline trace lexbuf }
| "@\"" { if mode = Copy then begin
             expand status offset; copy ignore lexbuf status;
             handle_err (pp_in_verb_str status) lexbuf
          end;
          pp_scan status env mode Inline trace lexbuf }
| "//"  { if mode = Copy then begin
             expand status offset; copy ignore lexbuf status;
             pp_in_line_com status mode lexbuf
          end;
          pp_scan status env mode Inline trace lexbuf }
| "/*"  { if mode = Copy then begin
             expand status offset; copy ignore lexbuf status;
             handle_err (pp_in_block_com status) lexbuf
          end;
          pp_scan status env mode Inline trace lexbuf }
| _     { if mode = Copy
          then (expand status offset; copy ignore lexbuf status);
          pp_scan status env mode Inline trace lexbuf }

(* Support for #define and #undef *)

and pp_ident status env = parse
  blank* { let r = __pp_ident env lexbuf
           in pp_newline status lexbuf; r }

and __pp_ident env = parse
  ident as id { id, Error.mk_seg lexbuf }

(* Line indicator (#line) *)

and pp_line_ind status = parse
  blank* as space {
    if status = Alone then print_string space;
    pp_line_indicator status lexbuf
  }

and pp_line_indicator status = parse
  decimal as ind {
    if status = Alone then print_string ind;
    pp_end_indicator status lexbuf;
    virt_lnum := int_of_string ind
  }
| ident as id {
    match id with
      "default" ->
        let msg = message [] lexbuf
        in if status = Alone
           then print_endline (id ^ msg);
           virt_lnum := lexbuf.Lexing.lex_curr_p.pos_lnum
    | "hidden" -> 
        let msg = message [] lexbuf
        in if status = Alone
           then print_endline (id ^ msg)
    | _ -> fail "Invalid line indicator." lexbuf
  }
| newline | eof { fail "Line indicator expected." lexbuf }

and pp_end_indicator status = parse
  blank* newline { handle_nl lexbuf; copy ignore lexbuf status }
| blank* eof     { copy ignore lexbuf status }
| blank* "//"    { copy ignore lexbuf status;
                   let msg = message [] lexbuf
                   in if status = Alone
                      then print_endline msg }
| blank+ '"'     { copy ignore lexbuf status;
                   handle_err (pp_in_norm_str status) lexbuf;
                   pp_opt_line_com status lexbuf }
| blank          { halt "Line comment or string expected." lexbuf }
| _              { fail "Blank expected." lexbuf }

and pp_opt_line_com status = parse
  newline { handle_nl lexbuf; copy ignore lexbuf status }
| eof     { copy ignore lexbuf status }
| blank+  { copy ignore lexbuf status; pp_opt_line_com status lexbuf }
| "//"    { let msg = message [] lexbuf
            in if status = Alone
               then print_endline ("//" ^ msg) }

(* New lines and verbatim sequence of characters *)

and pp_newline status = parse
  newline { handle_nl lexbuf; copy ignore lexbuf status }
| blank+  { pp_newline status lexbuf }
| "//"    { pp_in_line_com status Skip lexbuf }
| _       { fail "Only a single-line comment allowed." lexbuf }

and message acc = parse
  newline { handle_nl lexbuf;
            mk_str (List.length acc) acc }
| eof     { mk_str (List.length acc) acc }
| _ as c  { message (c::acc) lexbuf }

(* Comments *)

and pp_in_line_com status mode = parse
  newline { handle_nl lexbuf; copy ignore lexbuf status }
| eof     { if status = Alone then flush stdout }
| _       { if mode = Copy then copy ignore lexbuf status;
            pp_in_line_com status mode lexbuf }

and pp_in_block_com status = parse
  newline { handle_nl lexbuf; copy ignore lexbuf status;
            pp_in_block_com status lexbuf }
| "*/"    { copy ignore lexbuf status }
| eof     { raise (Local_err "Unterminated comment.") }
| _       { copy ignore lexbuf status;
            pp_in_block_com status lexbuf }

(* Strings *)

and pp_in_norm_str status = parse
  newline { fail "Newline invalid in string." lexbuf }
| '"'     { copy ignore lexbuf status }
| eof     { raise (Local_err "Unterminated string.") }
| _       { copy ignore lexbuf status; pp_in_norm_str status lexbuf }

and pp_in_verb_str status = parse
  newline { handle_nl lexbuf; copy ignore lexbuf status;
            pp_in_verb_str status lexbuf }
| '"'     { copy ignore lexbuf status }
| eof     { raise (Local_err "Unterminated string.") }
| _       { copy ignore lexbuf status; pp_in_verb_str status lexbuf }

(* Rules for the main lexer *)

(* val token : Lexing.lexbuf -> Parser.token * 'a as 'a *)

and token = parse
  newline      { handle_nl lexbuf; token lexbuf }
| blank+       { token lexbuf }

| '{'          { Parser.LBRACE,     token }
| '}'          { Parser.RBRACE,     token }
| '['          { Parser.LBRACKET,   token }
| ']'          { Parser.RBRACKET,   token }
| '('          { after_LPAR token lexbuf  }
| ')'          { Parser.RPAR,       token }
| '.'          { Parser.DOT,        token }
| ','          { Parser.COMMA,      token }
| ':'          { Parser.COLON,      token }
| ';'          { Parser.SEMI,       token }
| '+'          { Parser.PLUS,       token }
| '-'          { Parser.MINUS,      token }
| '*'          { Parser.TIMES,      token }
| '/'          { Parser.SLASH,      token }
| '%'          { Parser.PERCENT,    token }
| '&'          { Parser.AMPER,      token }
| '|'          { Parser.MID,        token }
| '^'          { Parser.CIRCUM,     token }
| '!'          { Parser.BANG,       token }
| '~'          { Parser.TILDE,      token }
| '='          { Parser.ASSIGN,     token }
| '<'          { Parser.LT,         token }
| '>'          { Parser.GT,         token }
| '?'          { Parser.QMARK,      token }
| "++"         { Parser.DPLUS,      token }
| "--"         { Parser.DMINUS,     token }
| "&&"         { Parser.DAMPER,     token }
| "||"         { Parser.DMID,       token }
| "<<"         { Parser.LSHIFT,     token }
| ">>"         { Parser.RSHIFT,     token }
| "=="         { Parser.EQ,         token }
| "!="         { Parser.NE,         token }
| "+="         { Parser.PLUSEQ,     token }
| "-="         { Parser.MINUSEQ,    token }
| "*="         { Parser.TIMESEQ,    token }
| "/="         { Parser.DIVEQ,      token }
| "%="         { Parser.PEREQ,      token }
| "&="         { Parser.AMPEREQ,    token }
| "|="         { Parser.MIDEQ,      token }
| "^="         { Parser.CIREQ,      token }
| "<<="        { Parser.LSHIFTEQ,   token }
| "<="         { Parser.LEQ,        token }
| ">>="        { Parser.RSHIFTEQ,   token }
| ">="         { Parser.GEQ,        token }
| "->"         { Parser.ARROW,      token }
| '"'          { handle_err (in_norm_str 0 []) lexbuf }
| "@\""        { handle_err (in_verb_str 0 []) lexbuf }
| "//"         { handle_err in_line_com lexbuf; token lexbuf  }
| "/*"         { handle_err in_block_com lexbuf; token lexbuf }
| eof          { Parser.EOF,        token }
| char as s    { Parser.Char s,     token }
| integer as s { Parser.Int s,      token }
| real as s    { Parser.Float s,    token }
| ident as s   { try Kwd.find s kwd_map, token with
                   Not_found -> Parser.Ident(Norm,s), token }
| '@' (ident as s)
               { Parser.Ident(Verb,s), token }

(*
| directive { (* TO DO *) }
*)
| _            { token lexbuf }

(* Comments *)

and in_line_com = parse
  newline { handle_nl lexbuf }
| eof     { () }
| _       { in_line_com lexbuf }
  
and in_block_com = parse
  newline { handle_nl lexbuf; in_block_com lexbuf }
| "*/"    { () }
| eof     { raise (Local_err "Unterminated comment.") }
| _       { in_block_com lexbuf }

(* Strings *)

and in_norm_str len acc = parse
  newline { raise (Local_err "Newline invalid in string.") }
| '"'     { Parser.String(Norm, mk_str len acc), token }
| eof     { raise (Local_err "Unterminated string.") }
| _ as c  { in_norm_str (len+1) (c::acc) lexbuf }

and in_verb_str len acc = parse
  newline as nl
          { handle_nl lexbuf;
            in_verb_str (len+1) (explode nl acc) lexbuf }
| '"'     { Parser.String(Verb, mk_str len acc), token }
| eof     { raise (Local_err "Unterminated string.") }
| _ as c  { in_verb_str (len+1) (c::acc) lexbuf }

{
(* The call [get_token buffer] returns a token, as expected by
   [menhir]. The parser cannot directly use the lexer [token] because
   it is streamlined. *)

let get_token : Lexing.lexbuf -> Parser.token =
  let strlex = ref token
in fun buffer -> let t, strlex' = !strlex buffer
                 in strlex := strlex'; t

(* Tracing the lexer without the preprocessor *)

type filename = string

let trace (name: filename) =
  match open_in name with
    cin ->
      let buffer = Lexing.from_channel cin in
      let rec iter lexer =
        match lexer buffer with
          Parser.EOF, _ -> close_in cin
        |     t, lexer' -> begin
                             print_endline (string_of_token t);
                             flush stdout;
                             iter lexer'
                           end
        | exception Error.Lexer diag ->
            close_in cin; Error.print "Lexical" diag
      in iter token
  | exception Sys_error msg -> prerr_endline msg

(* Tracing preprocessor expressions (after #if) *)

let ppe_trace (name: filename) =
  match open_in name with
    cin ->
      let buffer = Lexing.from_channel cin
      and cout = stdout in
      let rec iter () =
        match ppe_token buffer with
          Eparser.EOL -> close_in cin; close_out cout
        |           t -> begin
                           output_string cout (string_of_ppe_token t);
                           output_string cout "\n";
                           flush cout;
                           iter ()
                         end
        | exception Error.Lexer diag -> Error.print "Lexical" diag
      in iter ()
  | exception Sys_error msg -> prerr_endline msg

(* Tracing the preprocessor *)

let pp_lex status buffer =
  let _env, trace = pp_scan status Env.empty Copy (Prefix 0) [] buffer
in assert (trace = [])

let pp_trace status (name: filename) : unit =
  match open_in name with
    cin ->
      let buffer = Lexing.from_channel cin in
        let open Error
      in (try pp_lex status buffer with
            Lexer diag    -> print "Lexical" diag
          | Parser diag   -> print "Syntactical" diag
          | Eparser.Error -> print "" ("Parse", mk_seg buffer, !virt_lnum));
         close_in cin; flush stdout
  | exception Sys_error msg -> prerr_endline msg

}
