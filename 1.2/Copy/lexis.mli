(* Characters and strings can either be interpreted or taken
   verbatim. *)

type text_kind = Verb | Norm

(* Tokens *)

type token =
  ABSTRACT
| AS
| BASE
| BOOL
| BREAK
| BYTE
| CASE
| CATCH
| CHAR
| CHECKED
| CLASS
| CONST
| CONTINUE
| DECIMAL
| DEFAULT
| DELEGATE
| DO
| DOUBLE
| ELSE
| ENUM
| EVENT
| EXPLICIT
| EXTERN
| FALSE
| FINALLY
| FIXED
| FLOAT
| FOR
| FOREACH
| GOTO
| IF
| IMPLICIT
| IN
| INT
| INTERFACE
| INTERNAL
| IS
| LOCK
| LONG
| NAMESPACE
| NEW
| NULL
| OBJECT
| OPERATOR
| OUT
| OVERRIDE
| PARAMS
| PRIVATE
| PROTECTED
| PUBLIC
| READONLY
| REF
| RETURN
| SBYTE
| SEALED
| SHORT
| SIZEOF
| STACKALLOC
| STATIC
| STRING
| STRUCT
| SWITCH
| THIS
| THROW
| TRUE
| TRY
| TYPEOF
| UINT
| ULONG
| UNCHECKED
| UNSAFE
| USHORT
| USING
| VIRTUAL
| VOID
| VOLATILE
| WHILE

(* Symbols and operators *)

| LBRACE
| RBRACE
| LBRACKET
| RBRACKET
| LPAR
| RPAR
| DOT
| COMMA
| COLON
| SEMI

| PLUS
| MINUS
| TIMES
| SLASH
| PERCENT
| AMPER
| MID
| CIRCUM
| BANG
| TILDE

| ASSIGN
| LT
| GT
| QMARK
| DPLUS
| DMINUS
| DAMPER
| DMID
| LSHIFT
| RSHIFT

| EQ
| NE
| LEQ
| GEQ
| PLUSEQ
| MINUSEQ
| TIMESEQ
| DIVEQ
| PEREQ
| AMPEREQ

| MIDEQ
| CIREQ
| LSHIFTEQ
| RSHIFTEQ
| ARROW

(* Literals *)

| Ident  of text_kind * string
| Int    of string
| Float  of string
| Char   of string
| String of text_kind * string

(* Virtual tokens *)

| EOF
| CAST
