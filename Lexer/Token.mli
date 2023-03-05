type text_kind = Verb | Norm

type t =
  (* Keywords *)

  ABSTRACT | AS | BASE | BOOL | BREAK | BYTE | CASE | CATCH | CHAR | CHECKED
| CLASS | CONST | CONTINUE | DECIMAL | DEFAULT | DELEGATE | DO | DOUBLE | ELSE
| ENUM | EVENT | EXPLICIT | EXTERN | FALSE | FINALLY | FLOAT | FOR | FOREACH
| GOTO | IF | IMPLICIT | IN | INT | INTERFACE | INTERNAL | IS | LOCK | LONG
| NAMESPACE | NEW | NULL | OBJECT | OPERATOR | OUT | OVERRIDE | PARAMS
| PARTIAL | PRIVATE | PROTECTED | PUBLIC | READONLY | REF | RETURN | SBYTE
| SEALED | SHORT | STATIC | STRING | STRUCT | SWITCH | THIS| THROW | TRUE
| TRY | TYPEOF | UINT | ULONG | UNCHECKED | USHORT | USING | VIRTUAL | VOID
| VOLATILE | WHERE | WHILE | YIELD

(* Symbols and operators *)

| LBRACE    (* "{" *)
| RBRACE    (* "}" *)
| LBRACKET  (* "[" *)
| RBRACKET  (* "]" *)
| LPAR      (* "(" *)
| RPAR      (* ")" *)
| DOT       (* "." *)
| COMMA     (* "," *)
| COLON     (* ":" *)
| SEMI      (* ";" *)
| PLUS      (* "+" *)
| MINUS     (* "-" *)
| TIMES     (* "*" *)
| SLASH     (* "/" *)
| PERCENT   (* "%" *)
| AMPER     (* "&" *)
| MID       (* "|" *)
| CIRCUM    (* "^" *)
| BANG      (* "!" *)
| TILDE     (* "~" *)
| ASSIGN    (* "=" *)
| LT        (* "<" *)
| GT        (* ">" *)
| QMARK     (* "?" *)
| DQMARK    (* "??" *)
| DCOLON    (* "::" *)
| DPLUS     (* "++" *)
| DMINUS    (* "--" *)
| DAMPER    (* "&&" *)
| DMID      (* "||" *)
| EQ        (* "==" *)
| NE        (* "!=" *)
| LEQ       (* "<=" *)
| GEQ       (* ">=" *)
| PLUSEQ    (* "+=" *)
| MINUSEQ   (* "-=" *)
| TIMESEQ   (* "*=" *)
| DIVEQ     (* "/=" *)
| PEREQ     (* "%=" *)
| AMPEREQ   (* "&=" *)
| MIDEQ     (* "|=" *)
| CIREQ     (* "^=" *)
| LSHIFT    (* "<<" *)
| LSHIFTEQ  (* "<<=" *)

(* Literals *)

| Ident of text_kind * string
| Int of string
| Float of string
| Char of string
| String of text_kind * string

(* Virtual tokens *)

| NOSPACE
| EOF

type token = t

val to_string : t -> string
