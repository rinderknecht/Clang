{
 (* Ocamllex specification for C#
    ECMA-334, Annex A, page 443 (evince: p.465) *)

 (* Local errors *)

 exception Local_err of Error.message

 let handle_error scan buffer =
   let s = Error.mk_seg buffer
 in try scan buffer with Local_err msg -> raise (Error.Lexer (msg,s))

 (* String processing *)

 let mk_str (len:int) (p:char list) : string =
   let s = Bytes.make len ' ' in
   let rec fill i = function
       [] -> Bytes.to_string s
   | c::l -> Bytes.set s i c; fill (i-1) l
 in assert (len = List.length p); fill (len-1) p

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

| "abstract"   { [Token.ABSTRACT]   }
| "as"         { [Token.AS]         }
| "base"       { [Token.BASE]       }
| "bool"       { [Token.BOOL]       }
| "break"      { [Token.BREAK]      }
| "byte"       { [Token.BYTE]       }
| "case"       { [Token.CASE]       }
| "catch"      { [Token.CATCH]      }
| "char"       { [Token.CHAR]       }
| "checked"    { [Token.CHECKED]    }
| "class"      { [Token.CLASS]      }
| "const"      { [Token.CONST]      }
| "continue"   { [Token.CONTINUE]   }
| "decimal"    { [Token.DECIMAL]    }
| "default"    { [Token.DEFAULT]    }
| "delegate"   { [Token.DELEGATE]   }
| "do"         { [Token.DO]         }
| "double"     { [Token.DOUBLE]     }
| "else"       { [Token.ELSE]       }
| "enum"       { [Token.ENUM]       }
| "event"      { [Token.EVENT]      }
| "explicit"   { [Token.EXPLICIT]   }
| "extern"     { [Token.EXTERN]     }
| "false"      { [Token.FALSE]      }
| "finally"    { [Token.FINALLY]    }
| "float"      { [Token.FLOAT]      }
| "for"        { [Token.FOR]        }
| "foreach"    { [Token.FOREACH]    }
| "goto"       { [Token.GOTO]       }
| "if"         { [Token.IF]         }
| "implicit"   { [Token.IMPLICIT]   }
| "in"         { [Token.IN]         }
| "int"        { [Token.INT]        }
| "interface"  { [Token.INTERFACE]  }
| "internal"   { [Token.INTERNAL]   }
| "is"         { [Token.IS]         }
| "lock"       { [Token.LOCK]       }
| "long"       { [Token.LONG]       }
| "namespace"  { [Token.NAMESPACE]  }
| "new"        { [Token.NEW]        }
| "null"       { [Token.NULL]       }
| "object"     { [Token.OBJECT]     }
| "operator"   { [Token.OPERATOR]   }
| "out"        { [Token.OUT]        }
| "override"   { [Token.OVERRIDE]   }
| "params"     { [Token.PARAMS]     }
| "partial"    { [Token.PARTIAL]    }
| "private"    { [Token.PRIVATE]    }
| "protected"  { [Token.PROTECTED]  }
| "public"     { [Token.PUBLIC]     }
| "readonly"   { [Token.READONLY]   }
| "ref"        { [Token.REF]        }
| "return"     { [Token.RETURN]     }
| "sbyte"      { [Token.SBYTE]      }
| "sealed"     { [Token.SEALED]     }
| "short"      { [Token.SHORT]      }
| "static"     { [Token.STATIC]     }
| "string"     { [Token.STRING]     }
| "struct"     { [Token.STRUCT]     }
| "switch"     { [Token.SWITCH]     }
| "this"       { [Token.THIS]       }
| "throw"      { [Token.THROW]      }
| "true"       { [Token.TRUE]       }
| "try"        { [Token.TRY]        }
| "typeof"     { [Token.TYPEOF]     }
| "uint"       { [Token.UINT]       }
| "ulong"      { [Token.ULONG]      }
| "unchecked"  { [Token.UNCHECKED]  }
| "ushort"     { [Token.USHORT]     }
| "using"      { [Token.USING]      }
| "virtual"    { [Token.VIRTUAL]    }
| "void"       { [Token.VOID]       }
| "volatile"   { [Token.VOLATILE]   }
| "where"      { [Token.WHERE]      }
| "while"      { [Token.WHILE]      }
| "yield"      { [Token.YIELD]      }

| "{"          { [Token.LBRACE]   }
| "}"          { [Token.RBRACE]   }
| "["          { [Token.LBRACKET] }
| "]"          { [Token.RBRACKET] }
| "("          { [Token.LPAR]     }
| ")"          { [Token.RPAR]     }
| "."          { [Token.DOT]      }
| ","          { [Token.COMMA]    }
| ":"          { [Token.COLON]    }
| ";"          { [Token.SEMI]     }
| "+"          { [Token.PLUS]     }
| "-"          { [Token.MINUS]    }
| "*"          { [Token.TIMES]    }
| "/"          { [Token.SLASH]    }
| "%"          { [Token.PERCENT]  }
| "&"          { [Token.AMPER]    }
| "|"          { [Token.MID]      }
| "^"          { [Token.CIRCUM]   }
| "!"          { [Token.BANG]     }
| "~"          { [Token.TILDE]    }
| "="          { [Token.ASSIGN]   }

| "<"          { [Token.LT]       }
| "<<"         { [Token.LSHIFT]   }
| "<<="        { [Token.LSHIFTEQ] }
| "<="         { [Token.LEQ]      }

| ">"          { [Token.GT]                               }
| ">>"         { Token.[GT; NOSPACE; GT]                  }
| ">>="        { Token.[GT; NOSPACE; GT; NOSPACE; ASSIGN] }
| ">="         { Token.[GT; NOSPACE; ASSIGN]              }

| "?"          { [Token.QMARK]    }
| "??"         { [Token.DQMARK]   }
| "::"         { [Token.DCOLON]   }
| "++"         { [Token.DPLUS]    }
| "--"         { [Token.DMINUS]   }
| "&&"         { [Token.DAMPER]   }
| "||"         { [Token.DMID]     }
| "=="         { [Token.EQ]       }
| "!="         { [Token.NE]       }
| "+="         { [Token.PLUSEQ]   }
| "-="         { [Token.MINUSEQ]  }
| "*="         { [Token.TIMESEQ]  }
| "/="         { [Token.DIVEQ]    }
| "%="         { [Token.PEREQ]    }
| "&="         { [Token.AMPEREQ]  }
| "|="         { [Token.MIDEQ]    }
| "^="         { [Token.CIREQ]    }

| integer as s                { [Token.Int s]          }
| real as s                   { [Token.Float s]        }
| ident as s                  { Token.[Ident (Norm,s)] }
| '@' (ident as s)            { Token.[Ident (Verb,s)] }
| char as s                   { [Token.Char s]         }

| '"'   { handle_error (in_norm_str 0 []) lexbuf }
| "@\"" { handle_error (in_verb_str 0 []) lexbuf }

| "//" { handle_error in_line_comment lexbuf ;
         tokens lexbuf }
| "/*" { handle_error in_block_comment lexbuf;
         tokens lexbuf }

| eof { [Token.EOF] }
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
| '"'     { Token.[String (Norm, mk_str len acc)] }
| newline { raise (Local_err "Newline invalid in string.") }
| eof     { raise (Local_err "Unterminated string.") }
| _       { let chr = Lexing.lexeme_char lexbuf 0
            in in_norm_str (len+1) (chr::acc) lexbuf }

and in_verb_str len acc = parse
  "\"\""  { in_verb_str (len+1) ('"'::acc) lexbuf }
| '"'     { Token.[String (Verb, mk_str len acc)] }
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
              Token.EOF -> close_in cin; close_out cout
            | t -> begin
                     output_string cout (Token.to_string t);
                     output_string cout "\n";
                     flush cout;
                     iter ()
                   end
          with Error.Lexer diag -> Error.print "Lexical" diag
        in iter ()
  with Sys_error msg -> prerr_endline msg
}
