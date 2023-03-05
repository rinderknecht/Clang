{
(* Preprocessor for C# 1.2, to be processed by [ocamllex].
   See driver module [Topproc]. *)

(* Error handling *)

let stop msg seg = raise (Error.Lexer (msg, seg,1))
let fail msg buffer = stop msg (Error.mk_seg buffer)

exception Local_err of Error.message

let handle_err scan buffer =
  try scan buffer with Local_err msg -> fail msg buffer

(* String processing *)

let mk_str (len:int) (p:char list) : string =
  let s = Bytes.make len ' ' in 
  let rec fill i =
    function [] -> s | c::l -> Bytes.set s i c; fill (i-1) l
in assert (len = List.length p); Bytes.to_string (fill (len-1) p)

(* Copying the current lexeme to [stdout] *)

let copy buffer = print_string (Lexing.lexeme buffer)

(* End of lines *)

let handle_nl buffer = Lexing.new_line buffer; copy buffer

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

let expand = function Prefix 0 | Inline -> ()
                    | Prefix n -> print_string (String.make n ' ')

(* Directives *)

let directives = ["if"; "else"; "elif"; "endif"; "define"; "undef";
                  "error"; "warning"; "line"; "region"; "endregion"]

(* Environments and preprocessor expressions *)

module Env = Set.Make(String)

let rec eval env =
  let open Etree
in function
   Or (e1,e2) -> eval env e1 || eval env e2
| And (e1,e2) -> eval env e1 && eval env e2
|  Eq (e1,e2) -> eval env e1 = eval env e2
| Neq (e1,e2) -> eval env e1 != eval env e2
|       Not e -> not (eval env e)
|        True -> true
|       False -> false
|    Ident id -> Env.mem id env

let expr env buffer =
  let tree = Eparser.pp_expression Escan.token buffer
in if eval env tree then Copy else Skip

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

(* Directives *)

let directive = '#' (blank* as space) (ident as id)

(* Rules *)

(* The rule [scan] scans the input buffer for directives, strings,
   comments, blanks, new lines and end of file characters. As a
   result, either the matched input is copied to [stdout] or not,
   depending on the compilation directives. If not copied, new line
   characters are output.

   Scanning is triggered by the function call [scan env mode offset
   trace lexbuf], where [env] is the set of defined symbols
   (introduced by `#define'), [mode] specifies whether we are copying
   or skipping the input, [offset] informs about the location in the
   line (either there is a prefix of blanks, or at least a non-blank
   character has been read), and [trace] is the stack of conditional
   directives read so far.

   The first call is [scan Env.empty Copy (Prefix 0) []], meaning that
   we start with an empty environment, that copying the input is
   enabled by default, and that we are at the start of a line and no
   previous conditional directives have been read yet.

   When an "#if" is matched, the trace is extended by the call [extend
   lexbuf (If mode) trace], during the evaluation of which the
   syntactic validity of having encountered an "#if" is checked (for
   example, it would be invalid had an "#elif" been last read). Note
   that the current mode is stored in the trace with the current
   directive -- that mode may be later restored (see below for some
   examples). Moreover, the directive would be deemed invalid if its
   current position in the line (that is, its offset) were not
   preceeded by blanks or nothing, otherwise the rule [expr] is called
   to scan the boolean expression associated with the "#if": if it
   evaluates to [true], the result is [Copy], meaning that we may copy
   what follows, otherwise skip it -- the actual decision depending on
   the current mode. That new mode is used if we were in copy mode,
   and the offset is reset to the start of a new line (as we read a
   new line in [expr]); otherwise we were in skipping mode and the
   value of the conditional expression must be ignored (but not its
   syntax), and we continue skipping the input.

   When an "#else" is matched, the trace is extended with [Else],
   then, if the directive is not at a wrong offset, the rest of the
   line is scanned with [pp_newline]. If we were in copy mode, the new
   mode toggles to skipping mode; otherwise, the trace is searched for
   the last encountered "#if" of "#elif" and the associated mode is
   restored.

   The case "#elif" is the result of the fusion (in the technical
   sense) of the code for dealing with an "#else" followed by an
   "#if".

   When an "#endif" is matched, the trace is reduced, that is, all
   conditional directives are popped until an [If mode'] is found and
   [mode'] is restored as the current mode.

   Consider the following four cases, where the modes (Copy/Skip) are
   located between the lines:

                    Copy ----+                          Copy ----+
   #if true                  |       #if true                    |
                    Copy     |                          Copy     |
   #else                     |       #else                       |
                +-- Skip --+ |                      +-- Skip --+ |
     #if true   |          | |         #if false    |          | |
                |   Skip   | |                      |   Skip   | |
     #else      |          | |         #else        |          | |
                +-> Skip   | |                      +-> Skip   | |
     #endif                | |         #endif                  | |
                    Skip <-+ |                          Skip <-+ |
   #endif                    |       #endif                      |
                    Copy <---+                          Copy <---+


                +-- Copy ----+                          Copy --+-+
   #if false    |            |       #if false                 | |
                |   Skip     |                          Skip   | |
   #else        |            |       #else                     | |
                +-> Copy --+ |                    +-+-- Copy <-+ | 
     #if true              | |         #if false  | |            |
                    Copy   | |                    | |   Skip     |
     #else                 | |         #else      | |            |
                    Skip   | |                    | +-> Copy     |
     #endif                | |         #endif     |              |
                    Copy <-+ |                    +---> Copy     |
   #endif                    |       #endif                      |
                    Copy <---+                          Copy <---+

   The following four cases feature #elif. Note that we put between
   brackets the mode saved for the #elif, which is sometimes restored
   later.

                    Copy --+                            Copy --+
   #if true                |         #if true                  |
                    Copy   |                            Copy   |
   #elif true   +--[Skip]  |         #elif false    +--[Skip]  |
                |   Skip   |                        |   Skip   |
   #else        |          |         #else          |          |
                +-> Skip   |                        +-> Skip   |
   #endif                  |         #endif                    |
                    Copy <-+                            Copy <-+


                +-- Copy --+-+                      +-- Copy ----+
   #if false    |          | |       #if false      |            |
                |   Skip   | |                      |   Skip     |
   #elif true   +->[Copy]  | |       #elif false    +->[Copy]--+ |
                    Copy <-+ |                          Skip   | |
   #else                     |       #else                     | |
                    Skip     |                          Copy <-+ |
   #endif                    |       #endif                      |
                    Copy <---+                          Copy <---+

   Note how "#elif" indeed behaves like an "#else" followed by an
   "#if", and the mode stored with the data constructor [Elif]
   corresponds to the mode before the virtual "#if".

   Important note: Comments and strings are recognised as such only in
   copy mode, which is a different behaviour from the preprocessor of
   GNU GCC, which always does.
*)

rule scan env mode offset trace = parse
  newline   { handle_nl lexbuf;
              scan env mode (Prefix 0) trace lexbuf }
| blank     { match offset with
                Prefix n -> scan env mode (Prefix (n+1)) trace lexbuf
              |   Inline -> copy lexbuf;
                            scan env mode Inline trace lexbuf }
| directive {
    if not (List.mem id directives)
    then fail "Invalid preprocessing directive." lexbuf
    else if offset = Inline
         then fail "Directive invalid inside line." lexbuf
         else let seg = Error.mk_seg lexbuf in 
    match id with
      "if" ->
        let mode' = expr env lexbuf in
        let new_mode = if mode = Copy then mode' else Skip in
        let trace' = extend seg (If mode) trace
        in scan env new_mode (Prefix 0) trace' lexbuf
    | "else" ->
        let () = pp_newline lexbuf in
        let new_mode =
          if mode = Copy then Skip else last_mode trace in
        let trace' = extend seg Else trace
        in scan env new_mode (Prefix 0) trace' lexbuf
    | "elif" ->
        let mode' = expr env lexbuf in
        let trace', new_mode =
          match mode with
            Copy -> extend seg (Elif Skip) trace, Skip
          | Skip -> let old_mode = last_mode trace
                    in extend seg (Elif old_mode) trace,
                       if old_mode = Copy then mode' else Skip
        in scan env new_mode (Prefix 0) trace' lexbuf
    | "endif" ->
        let () = pp_newline lexbuf in
        let trace', new_mode = reduce_cond seg trace
        in scan env new_mode (Prefix 0) trace' lexbuf
    | "define" ->
        let id, seg = ident env lexbuf
        in if id="true" || id="false"
           then let msg = "Symbol \"" ^ id ^ "\" cannot be defined."
                in stop msg seg
           else if Env.mem id env
                then let msg = "Symbol \"" ^ id
                               ^ "\" was already defined."
                     in stop msg seg
                else scan (Env.add id env) mode (Prefix 0) trace lexbuf
    | "undef" ->
        let id, _ = ident env lexbuf
        in scan (Env.remove id env) mode (Prefix 0) trace lexbuf
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
           scan env mode (Prefix 0) trace lexbuf
    | "region" ->
        let msg = message [] lexbuf
        in expand offset;
           print_endline ("#" ^ space ^ "region" ^ msg);
           scan env mode (Prefix 0) (Region::trace) lexbuf
    | "endregion" ->
        let msg = message [] lexbuf
        in expand offset;
           print_endline ("#" ^ space ^ "endregion" ^ msg);
           scan env mode (Prefix 0) (reduce_reg seg trace) lexbuf
    | "line" ->
        expand offset;
        print_string ("#" ^ space ^ "line");
        line_ind lexbuf;
        scan env mode (Prefix 0) trace lexbuf
    | _ -> assert false 
  }
| eof   { match trace with
            [] -> expand offset; flush stdout; (env, trace) 
          |  _ -> fail "Missing #endif." lexbuf }
| '"'   { if mode = Copy then begin
             expand offset; copy lexbuf;
             handle_err in_norm_str lexbuf
          end;
          scan env mode Inline trace lexbuf }
| "@\"" { if mode = Copy then begin
             expand offset; copy lexbuf;
             handle_err in_verb_str lexbuf
          end;
          scan env mode Inline trace lexbuf }
| "//"  { if mode = Copy then begin
             expand offset; copy lexbuf;
             in_line_com mode lexbuf
          end;
          scan env mode Inline trace lexbuf }
| "/*"  { if mode = Copy then begin
             expand offset; copy lexbuf;
             handle_err in_block_com lexbuf
          end;
          scan env mode Inline trace lexbuf }
| _     { if mode = Copy then (expand offset; copy lexbuf);
          scan env mode Inline trace lexbuf }

(* Support for #define and #undef *)

and ident env = parse
  blank* { let r = __ident env lexbuf
           in pp_newline lexbuf; r }

and __ident env = parse
  ident as id { id, Error.mk_seg lexbuf }

(* Line indicator (#line) *)

and line_ind = parse
  blank* as space { print_string space; line_indicator lexbuf }

and line_indicator = parse
  decimal as ind {
    print_string ind;
    end_indicator lexbuf
  }
| ident as id {
    match id with
      "default" | "hidden" ->
        print_endline (id ^ message [] lexbuf)
    | _ -> fail "Invalid line indicator." lexbuf
  }
| newline | eof { fail "Line indicator expected." lexbuf }

and end_indicator = parse
  blank* newline { copy lexbuf; handle_nl lexbuf }
| blank* eof     { copy lexbuf }
| blank* "//"    { copy lexbuf; print_endline (message [] lexbuf) }
| blank+ '"'     { copy lexbuf;
                   handle_err in_norm_str lexbuf;
                   opt_line_com lexbuf }
| _              { fail "Line comment or blank expected." lexbuf }

and opt_line_com = parse
  newline { handle_nl lexbuf }
| eof     { copy lexbuf }
| blank+  { copy lexbuf; opt_line_com lexbuf }
| "//"    { print_endline ("//" ^ message [] lexbuf) }

(* New lines and verbatim sequence of characters *)

and pp_newline = parse
  newline { handle_nl lexbuf }
| blank+  { pp_newline lexbuf }
| "//"    { in_line_com Skip lexbuf }
| _       { fail "Only a single-line comment allowed." lexbuf }

and message acc = parse
  newline { Lexing.new_line lexbuf; 
            mk_str (List.length acc) acc }
| eof     { mk_str (List.length acc) acc }
| _ as c  { message (c::acc) lexbuf }

(* Comments *)

and in_line_com mode = parse
  newline { handle_nl lexbuf }
| eof     { flush stdout }
| _       { if mode = Copy 
            then copy lexbuf; in_line_com mode lexbuf }

and in_block_com = parse
  newline { handle_nl lexbuf; in_block_com lexbuf }
| "*/"    { copy lexbuf }
| eof     { raise (Local_err "Unterminated comment.") }
| _       { copy lexbuf; in_block_com lexbuf }

(* Strings *)

and in_norm_str = parse
  "\\\""  { copy lexbuf; in_norm_str lexbuf }
| '"'     { copy lexbuf }
| newline { fail "Newline invalid in string." lexbuf }
| eof     { raise (Local_err "Unterminated string.") }
| _       { copy lexbuf; in_norm_str lexbuf }

and in_verb_str = parse
  "\"\""  { copy lexbuf; in_verb_str lexbuf }
| '"'     { copy lexbuf }
| newline { handle_nl lexbuf; in_verb_str lexbuf }
| eof     { raise (Local_err "Unterminated string.") }
| _       { copy lexbuf; in_verb_str lexbuf }

{
(* Internal definitions *)

let lex buffer =
  let _env, trace = scan Env.empty Copy (Prefix 0) [] buffer
in assert (trace = [])

(* Exported definitions *)

type filename = string

let trace (name: filename) : unit =
  match open_in name with
    cin ->
      let buffer = Lexing.from_channel cin in
        let open Error
      in (try lex buffer with
            Lexer diag    -> print "Lexical" diag
          | Parser diag   -> print "Syntactical" diag
          | Eparser.Error -> print "" ("Parse", mk_seg buffer, 1));
         close_in cin; flush stdout
  | exception Sys_error msg -> prerr_endline msg

}
