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

(* Concrete syntax of tokens. *)

let to_string = function
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

| Ident (Norm,s)  -> "Ident(" ^ s ^ ")"
| Ident (Verb,s)  -> "Ident(@" ^ s ^ ")"
| Int s           -> "Int(" ^ s ^ ")"
| Float s         -> "Float(" ^ s ^ ")"
| Char s          -> "Char(" ^ s ^ ")"
| String (Norm,s) -> "String(" ^ s ^ ")"
| String (Verb,s) -> "String(@" ^ s ^ ")"

(* Virtual tokens *)

| NOSPACE -> "NOSPACE"
| EOF     -> "EOF"
