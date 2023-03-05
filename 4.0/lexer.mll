{
 (* Ocamllex specification for C#
    ECMA-334, Annex A, page 443 (evince: p.465) *)

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
| PARTIAL    -> "partial"
| PRIVATE    -> "private"
| PROTECTED  -> "protected"
| PUBLIC     -> "public"
| READONLY   -> "readonly"
| REF        -> "ref"
| RETURN     -> "return"
| SBYTE      -> "sbyte"
| SEALED     -> "sealed"
| SHORT      -> "short"
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
| USHORT     -> "ushort"
| USING      -> "using"
| VIRTUAL    -> "virtual"
| VOID       -> "void"
| VOLATILE   -> "volatile"
| WHERE      -> "where"
| WHILE      -> "while"
| YIELD      -> "yield"

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
| DCOLON    -> "::"
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
| "break"      { [Parser.BREAK]      }
| "byte"       { [Parser.BYTE]       }
| "case"       { [Parser.CASE]       }
| "catch"      { [Parser.CATCH]      }
| "char"       { [Parser.CHAR]       }
| "checked"    { [Parser.CHECKED]    }
| "class"      { [Parser.CLASS]      }
| "const"      { [Parser.CONST]      }
| "continue"   { [Parser.CONTINUE]   }
| "decimal"    { [Parser.DECIMAL]    }
| "default"    { [Parser.DEFAULT]    }
| "delegate"   { [Parser.DELEGATE]   }
| "do"         { [Parser.DO]         }
| "double"     { [Parser.DOUBLE]     }
| "else"       { [Parser.ELSE]       }
| "enum"       { [Parser.ENUM]       }
| "event"      { [Parser.EVENT]      }
| "explicit"   { [Parser.EXPLICIT]   }
| "extern"     { [Parser.EXTERN]     }
| "false"      { [Parser.FALSE]      }
| "finally"    { [Parser.FINALLY]    }
| "float"      { [Parser.FLOAT]      }
| "for"        { [Parser.FOR]        }
| "foreach"    { [Parser.FOREACH]    }
| "goto"       { [Parser.GOTO]       }
| "if"         { [Parser.IF]         }
| "implicit"   { [Parser.IMPLICIT]   }
| "in"         { [Parser.IN]         }
| "int"        { [Parser.INT]        }
| "interface"  { [Parser.INTERFACE]  }
| "internal"   { [Parser.INTERNAL]   }
| "is"         { [Parser.IS]         }
| "lock"       { [Parser.LOCK]       }
| "long"       { [Parser.LONG]       }
| "namespace"  { [Parser.NAMESPACE]  }
| "new"        { [Parser.NEW]        }
| "null"       { [Parser.NULL]       }
| "object"     { [Parser.OBJECT]     }
| "operator"   { [Parser.OPERATOR]   }
| "out"        { [Parser.OUT]        }
| "override"   { [Parser.OVERRIDE]   }
| "params"     { [Parser.PARAMS]     }
| "partial"    { [Parser.PARTIAL]    }
| "private"    { [Parser.PRIVATE]    }
| "protected"  { [Parser.PROTECTED]  }
| "public"     { [Parser.PUBLIC]     }
| "readonly"   { [Parser.READONLY]   }
| "ref"        { [Parser.REF]        }
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
| "typeof"     { [Parser.TYPEOF]     }
| "uint"       { [Parser.UINT]       }
| "ulong"      { [Parser.ULONG]      }
| "unchecked"  { [Parser.UNCHECKED]  }
| "ushort"     { [Parser.USHORT]     }
| "using"      { [Parser.USING]      }
| "virtual"    { [Parser.VIRTUAL]    }
| "void"       { [Parser.VOID]       }
| "volatile"   { [Parser.VOLATILE]   }
| "where"      { [Parser.WHERE]      }
| "while"      { [Parser.WHILE]      }
| "yield"      { [Parser.YIELD]      }

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
| "::"         { [Parser.DCOLON]   }
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
