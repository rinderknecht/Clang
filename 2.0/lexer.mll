{
 (* Ocamllex specification for C# 2.0 *)

 (* Local errors *)

 exception Local_err of Error.message

 let lex_err msg buffer =
   raise (Error.Lexer (msg, Error.mk_seg buffer))

 let handle_error scan buffer =
   let s = Error.mk_seg buffer
 in try scan buffer with Local_err msg -> raise (Error.Lexer (msg,s))

 (* String processing *)

 let mk_str (len:int) (p:char list) : string =
   let s = String.make len ' ' in 
   let rec fill i =
     function [] -> s | c::l -> s.[i] <- c; fill (i-1) l
 in assert (len = List.length p); fill (len-1) p

(* Concrete syntax of tokens. See module [Parser]. *)

let string_of_token = 
  let open Parser in function
  (* Keywords *)

  ABSTRACT      -> "abstract"
| AS            -> "as"
| BASE          -> "base"
| BOOL          -> "bool"
| BYTE          -> "byte"
| CASE          -> "case"
| CATCH         -> "catch"
| CHAR          -> "char"
| CLASS         -> "class"
| CONST         -> "const"
| DEFAULT_COLON -> "default:"
| DEFAULT_LPAR  -> "default ("
| DO            -> "do"
| DOUBLE        -> "double"
| ELSE          -> "else"
| FALSE         -> "false"
| FINALLY       -> "finally"
| FLOAT         -> "float"
| FOR           -> "for"
| IF            -> "if"
| INT           -> "int"
| INTERFACE     -> "interface"
| INTERNAL      -> "internal"
| IS            -> "is"
| LONG          -> "long"
| NEW           -> "new"
| NULL          -> "null"
| OBJECT        -> "object"
| OVERRIDE      -> "override"
| PRIVATE       -> "private"
| PROTECTED     -> "protected"
| PUBLIC        -> "public"
| READONLY      -> "readonly"
| RETURN        -> "return"
| SBYTE         -> "sbyte"
| SEALED        -> "sealed"
| SHORT         -> "short"
| STATIC        -> "static"
| STRING        -> "string"
| STRUCT        -> "struct"
| SWITCH        -> "switch"
| THIS          -> "this"
| THROW         -> "throw"
| TRUE          -> "true"
| TRY           -> "try"
| UINT          -> "uint"
| ULONG         -> "ulong"
| USHORT        -> "ushort"
| VIRTUAL       -> "virtual"
| VOID          -> "void"
| WHERE         -> "where"
| WHILE         -> "while"

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
| DQMARK    -> "??"
| DPLUS     -> "++"
| DMINUS    -> "--"
| DAMPER    -> "&&"
| DMID      -> "||"
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
| LSHIFT    -> "<<"
| LSHIFTEQ  -> "<<="

(* Literals *)

| Ident (Lexis.Norm,s)  -> "Ident(" ^ s ^ ")"
| Ident (Lexis.Verb,s)  -> "Ident(@" ^ s ^ ")"
| Int s                 -> "Int(" ^ s ^ ")"
| Float s               -> "Float(" ^ s ^ ")" 
| Char s                -> "Char(" ^ s ^ ")"
| String (Lexis.Norm,s) -> "String(" ^ s ^ ")"
| String (Lexis.Verb,s) -> "String(@" ^ s ^ ")"

(* Special tokens *)

| NOSPACE -> "NOSPACE"
| EOF     -> "EOF"

}

(* Regular expressions for litterals *)

(* White space (9.3.3, p.69, evince: p.91). *)

let newline = '\n' | '\r' (* | "\r\n" *)
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

(* Unicode escape sequences (9.4.1, p.69, evince: p.91) *)

let four_hex = hexdigit hexdigit hexdigit hexdigit
let uni_esc = "\\u" four_hex | "\\U"  four_hex four_hex

(* Identifiers (9.4.2, p.70, evince: p.92) *)

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letter = lowercase | uppercase | uni_esc
let start = '_' | letter
let alphanum = letter | digit | '_'
let ident = start alphanum*

(* Real (9.4.4.3, p.73, evince: p.95) *)

let decimal = digit+
let exponent = (['e' 'E'] ['+' '-']? decimal)?
let real_suf = ['F' 'f' 'D' 'd' 'M' 'm']
let real = (decimal? '.')? decimal exponent? real_suf?

(* Characters (9.4.4.4, p.74, evince: p.96) *)

let single = [^ '\n' '\r']
let esc = "\\'" | "\\\"" | "\\\\" | "\\0" | "\\a" | "\\b" | "\\f"
        | "\\n" | "\\r" | "\\t" | "\\v"
let hex_esc = "\\x" hexdigit hexdigit? hexdigit? hexdigit?
let character = single | esc | hex_esc | uni_esc
let char = "'" character "'"

(* Rules *)

rule tokens = parse
  newline      { Lexing.new_line lexbuf; tokens lexbuf }
| blank+       { tokens lexbuf }

| "abstract"   { [Parser.ABSTRACT]   }
| "as"         { [Parser.AS]         }
| "base"       { [Parser.BASE]       }
| "bool"       { [Parser.BOOL]       }
| "byte"       { [Parser.BYTE]       }
| "case"       { [Parser.CASE]       }
| "catch"      { [Parser.CATCH]      }
| "char"       { [Parser.CHAR]       }
| "class"      { [Parser.CLASS]      }
| "const"      { [Parser.CONST]      }
| "default"    { let open Parser
                 in match tokens lexbuf with
                      [COLON] -> [DEFAULT_COLON]
                    |  [LPAR] -> [DEFAULT_LPAR]
                    |       _ -> raise (Local_err "The default keyword must be followed by a colon or an opening parenthesis.")
               }
| "do"         { [Parser.DO]         }
| "double"     { [Parser.DOUBLE]     }
| "else"       { [Parser.ELSE]       }
| "false"      { [Parser.FALSE]      }
| "finally"    { [Parser.FINALLY]    }
| "float"      { [Parser.FLOAT]      }
| "for"        { [Parser.FOR]        }
| "if"         { [Parser.IF]         }
| "int"        { [Parser.INT]        }
| "interface"  { [Parser.INTERFACE]  }
| "internal"   { [Parser.INTERNAL]   }
| "is"         { [Parser.IS]         }
| "long"       { [Parser.LONG]       }
| "new"        { [Parser.NEW]        }
| "null"       { [Parser.NULL]       }
| "object"     { [Parser.OBJECT]     }
| "override"   { [Parser.OVERRIDE]   }
| "private"    { [Parser.PRIVATE]    }
| "protected"  { [Parser.PROTECTED]  }
| "public"     { [Parser.PUBLIC]     }
| "readonly"   { [Parser.READONLY]   }
| "return"     { [Parser.RETURN]     }
| "sbyte"      { [Parser.SBYTE]      }
| "sealed"     { [Parser.SEALED]     }
| "short"      { [Parser.SHORT]      }
| "static"     { [Parser.STATIC]     }
| "string"     { [Parser.STRING]     }
| "struct"     { [Parser.STRUCT]     }
| "switch"     { [Parser.SWITCH]     }
| "this"       { [Parser.THIS]       }
| "throw"      { [Parser.THROW]      }
| "true"       { [Parser.TRUE]       }
| "try"        { [Parser.TRY]        }
| "uint"       { [Parser.UINT]       }
| "ulong"      { [Parser.ULONG]      }
| "ushort"     { [Parser.USHORT]     }
| "virtual"    { [Parser.VIRTUAL]    }
| "void"       { [Parser.VOID]       }
| "where"      { [Parser.WHERE]      }
| "while"      { [Parser.WHILE]      }

| "{"          { [Parser.LBRACE]   }
| "}"          { [Parser.RBRACE]   }
| "["          { [Parser.LBRACKET] }
| "]"          { [Parser.RBRACKET] }
| "("          { [Parser.LPAR]     }
| ")"          { [Parser.RPAR]     }
| "."          { [Parser.DOT]      }
| ","          { [Parser.COMMA]    }
| ":"          { [Parser.COLON]    }
| ";"          { [Parser.SEMI]     }
| "+"          { [Parser.PLUS]     }
| "-"          { [Parser.MINUS]    }
| "*"          { [Parser.TIMES]    }
| "/"          { [Parser.SLASH]    }
| "%"          { [Parser.PERCENT]  }
| "&"          { [Parser.AMPER]    }
| "|"          { [Parser.MID]      }
| "^"          { [Parser.CIRCUM]   }
| "!"          { [Parser.BANG]     }
| "~"          { [Parser.TILDE]    }
| "="          { [Parser.ASSIGN]   }

| "<"          { [Parser.LT]       }
| "<<"         { [Parser.LSHIFT]   }
| "<<="        { [Parser.LSHIFTEQ] }
| "<="         { [Parser.LEQ]      }

| ">"          { [Parser.GT]       }
| ">>"         { let open Parser in [GT; NOSPACE; GT] }
| ">>="        { let open Parser
                 in [GT; NOSPACE; GT; NOSPACE; ASSIGN] }
| ">="         { let open Parser in [GT; NOSPACE; ASSIGN] }

| "?"          { [Parser.QMARK]    }
| "??"         { [Parser.DQMARK]   }
| "++"         { [Parser.DPLUS]    }
| "--"         { [Parser.DMINUS]   }
| "&&"         { [Parser.DAMPER]   }
| "||"         { [Parser.DMID]     }
| "=="         { [Parser.EQ]       }
| "!="         { [Parser.NE]       }
| "+="         { [Parser.PLUSEQ]   }
| "-="         { [Parser.MINUSEQ]  }
| "*="         { [Parser.TIMESEQ]  }
| "/="         { [Parser.DIVEQ]    }
| "%="         { [Parser.PEREQ]    }
| "&="         { [Parser.AMPEREQ]  }
| "|="         { [Parser.MIDEQ]    }
| "^="         { [Parser.CIREQ]    }

| integer as s                { [Parser.Int s]          }
| real as s                   { [Parser.Float s]        }
| ident as s                  { [Parser.Ident(Norm,s)]  }
| '@' (ident as s)            { [Parser.Ident(Verb,s)]  }
| char as s                   { [Parser.Char s]         }

| '"'   { handle_error (in_norm_str 0 []) lexbuf }
| "@\"" { handle_error (in_verb_str 0 []) lexbuf }

| "//" { handle_error in_line_comment lexbuf ;
         tokens lexbuf }
| "/*" { handle_error in_block_comment lexbuf;
         tokens lexbuf }

| eof { [Parser.EOF] }
| _   { let open Error
        in raise (Lexer ("Invalid character.", mk_seg lexbuf)) }

and in_line_comment = parse
  newline { Lexing.new_line lexbuf }
| eof     { () }
| _       { in_line_comment lexbuf }
  
and in_block_comment = parse
  newline { Lexing.new_line lexbuf }
| "*/"    { () }
| eof     { raise (Local_err "Unterminated comment.") }
| _       { in_block_comment lexbuf }

and in_norm_str len acc = parse
  "\"\""  { in_norm_str (len+1) ('"'::acc) lexbuf }
| '"'     { [Parser.String(Norm, mk_str len acc)] }
| newline { raise (Local_err "Newline invalid in string.") }
| eof     { raise (Local_err "Unterminated string.") } 
| _       { let chr = Lexing.lexeme_char lexbuf 0
            in in_norm_str (len+1) (chr::acc) lexbuf }

and in_verb_str len acc = parse
  "\"\""  { in_verb_str (len+1) ('"'::acc) lexbuf }
| '"'     { [Parser.String(Verb, mk_str len acc)] }
| newline { Lexing.new_line lexbuf;
            let chr = Lexing.lexeme_char lexbuf 0
            in in_verb_str (len+1) (chr::acc) lexbuf }
| eof     { raise (Local_err "Unterminated string.") } 
| _       { let chr = Lexing.lexeme_char lexbuf 0
            in in_verb_str (len+1) (chr::acc) lexbuf }

{
(* Standalone lexer for debugging purposes *)

type filename = string

let rec token =
  let store = ref []
in fun buf -> match !store with
                     [] -> store := tokens buf; token buf
              | t::more -> store := more; t

let trace (name: filename) =
  try
    match open_in name with
      cin ->
        let buffer = Lexing.from_channel cin in
        let cout = stdout in
        let rec iter () =
          try
            match token buffer with
              Parser.EOF -> close_in cin; close_out cout
            | t -> begin
                     output_string cout (string_of_token t);
                     output_string cout "\n";
                     flush cout;
                     iter ()
                   end
          with Error.Lexer diag -> Error.print "Lexical" diag
        in iter ()
  with Sys_error msg -> prerr_endline msg
}
