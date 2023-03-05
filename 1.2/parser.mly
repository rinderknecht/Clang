%{
(* Grammar for C# 1.2 *)
%}

(* Keywords *)

%token ABSTRACT AS       BASE       BOOL      BREAK
%token BYTE     CASE     CATCH      CHAR      CHECKED
%token CLASS    CONST    CONTINUE   DECIMAL   DEFAULT
%token DELEGATE DO       DOUBLE     ELSE      ENUM
%token EVENT    EXPLICIT EXTERN     FALSE     FINALLY
%token FIXED    FLOAT    FOR        FOREACH   GOTO
%token IF       IMPLICIT IN         INT       INTERFACE
%token INTERNAL IS       LOCK       LONG      NAMESPACE
%token NEW      NULL     OBJECT     OPERATOR  OUT
%token OVERRIDE PARAMS   PRIVATE    PROTECTED PUBLIC
%token READONLY REF      RETURN     SBYTE     SEALED
%token SHORT    SIZEOF   STACKALLOC STATIC    STRING
%token STRUCT   SWITCH   THIS       THROW     TRUE
%token TRY      TYPEOF   UINT       ULONG     UNCHECKED
%token UNSAFE   USHORT   USING      VIRTUAL   VOID
%token VOLATILE WHILE

(* Symbols *)

%token LBRACE    (* {   *)
%token RBRACE    (* }   *)
%token LBRACKET  (* [   *)
%token RBRACKET  (* ]   *)
%token LPAR      (* (   *)
%token RPAR      (* )   *)
%token DOT       (* .   *)
%token COMMA     (* ,   *)
%token COLON     (* :   *)
%token SEMI      (* ;   *)

%token PLUS      (* +   *)
%token MINUS     (* -   *)
%token TIMES     (* *   *)
%token SLASH     (* /   *)
%token PERCENT   (* %   *)
%token AMPER     (* &   *)
%token MID       (* |   *)
%token CIRCUM    (* ^   *)
%token BANG      (* !   *)
%token TILDE     (* ~   *)

%token ASSIGN    (* =   *)
%token LT        (* <   *)
%token GT        (* >   *)
%token QMARK     (* ?   *)
%token DPLUS     (* ++  *)
%token DMINUS    (* --  *)
%token DAMPER    (* &&  *)
%token DMID      (* ||  *)
%token LSHIFT    (* <<  *)
%token RSHIFT    (* >>  *)

%token EQ        (* ==  *)
%token NE        (* !=  *)
%token LEQ       (* <=  *)
%token GEQ       (* >=  *)
%token PLUSEQ    (* +=  *)
%token MINUSEQ   (* -=  *)
%token TIMESEQ   (* *=  *)
%token DIVEQ     (* /=  *)
%token PEREQ     (* %=  *)
%token AMPEREQ   (* &=  *)

%token MIDEQ     (* |=  *)
%token CIREQ     (* ^=  *)
%token LSHIFTEQ  (* <<= *)
%token RSHIFTEQ  (* >>= *)
%token ARROW     (* ->  *)

(* Identifiers and literals *)

%token <Lexis.text_kind * string> Ident
%token <Lexis.text_kind * string> String
%token <string> Int
%token <string> Float
%token <string> Char

(* Virtual tokens *)

%token CAST
%token EOF

(* Entries *)

%start main
%type <unit> main

%%

(* Grammar *)

(* Entry point *)

main:
  compilation_unit EOF {}

(* Namespaces *)

compilation_unit:
  option(using_directives) option(namespace_member_declarations) {}

namespace_declaration:
  NAMESPACE qualified_identifier namespace_body option(SEMI) {}

qualified_identifier:
  Ident {}
| qualified_identifier DOT Ident {}

namespace_body:
  LBRACE option(using_directives)
         option(namespace_member_declarations) RBRACE {}

using_directives:
  using_directive {}
| using_directives using_directive {}

using_directive:
  USING Ident ASSIGN qualified_identifier SEMI { (* Alias *)
    (* [qualified_identifier] is a [namespace_or_type_name]. *)
  }
| USING qualified_identifier SEMI {
    (* [qualified_identifier] is a [namespace_name]. *)
  }

namespace_member_declarations:
  namespace_member_declaration {}
| namespace_member_declarations namespace_member_declaration {}

namespace_member_declaration:
  namespace_declaration {}
| type_declaration {
  (* If the [type_declaration] is the first in the file, if a prefix
     of its attributes contain Ident="assembly" or Ident="module",
     then these attributes are global, not local, that is, they apply
     to the file, not the declaration at hand. *)
  }

type_declaration:
  class_declaration {}
| struct_declaration {}
| interface_declaration {}
| enum_declaration {}
| delegate_declaration {}

(* Types *)

_type:
  non_array_type {
   (* In [class_type], [qualified_identifier] can be a
      [delegate_type], an [qualified_identifier], an [enum_type] or a
      [struct_type]. *)
  }
| array_type {} 

pointer_type:
  __pointed_type __stars {}

__pointed_type:
  simple_type {}
| class_type {
   (* [qualified_identifier] from [class_type] can be a [class_type],
      a [delegate_type], an [interface_type], an [enum_type] or a
      [struct_type]. *)
  }
| VOID {}

__stars:
  TIMES {}
| __stars TIMES {}

integral_type:
  SBYTE {}
| BYTE {}
| SHORT {}
| USHORT {}
| INT {}
| UINT {}
| LONG {}
| ULONG {}
| CHAR {}

simple_type:
  integral_type {}
| FLOAT {}
| DOUBLE {}
| DECIMAL {}
| BOOL {}

class_type:
  qualified_identifier {}
| OBJECT {}
| STRING {}

array_type:
  simple_type rank_specifiers {}
| pointer_type rank_specifiers{}
| qualified_identifier rank_specifiers {}
| OBJECT rank_specifiers {}
| STRING rank_specifiers {}

(* In [non_array_type], [qualified_identifier] can be a [class_type],
   a [delegate_type], an [interface_type], an [enum_type] or a
   [struct_type]. *)

%inline non_array_type:
  simple_type {}
| pointer_type {}
| qualified_identifier {}
| OBJECT {}
| STRING {}

rank_specifiers:
  rank_specifier {}
| rank_specifiers rank_specifier {}

rank_specifier:
  LBRACKET option(dim_separators) RBRACKET {}

dim_separators:
  COMMA {}
| dim_separators COMMA {}

(* Expressions *)

argument_list:
  argument {}
| argument_list COMMA argument {}

argument:
  expression {}
| REF expression {}
| OUT expression {}

primary_expression:
  _primary_no_array_creation_expression {}
| array_creation_expression {}
| qualified_identifier {}

_primary_no_array_creation_expression:
  TRUE {}
| FALSE {}
| Int {}
| Float {}
| Char {}
| String {}
| NULL {}
| LPAR expression_list RPAR {}
| member_access {}
| invocation_expression {}
| element_access {
  (* If only one expression, [element_access] can also be a
     [pointer_element_access]. *)
  }
| THIS {}
| base_access {}
| post_increment_expression {}
| post_decrement_expression {}
| object_creation_expression {
  (* If [NEW qualified_identifier LPAR expression RPAR],
     then it can be a [delegate_creation_expression]. *)
  } 
| typeof_expression {}
| CHECKED LPAR expression RPAR {}
| UNCHECKED LPAR expression RPAR {}
| pointer_member_access {}
| SIZEOF LPAR _type RPAR { (* [_type] is [unmanaged_type] *) }
 
member_access:
  __primary_expression DOT Ident {}
| predefined_type DOT Ident {}

__primary_expression:
  __primary_no_array_creation_expression {}
| array_creation_expression {}

__primary_no_array_creation_expression:
  TRUE {}
| FALSE {}
| Int {}
| Float {}
| Char {}
| String {}
| NULL {}
| LPAR expression_list RPAR {}
| member_access {}
| invocation_expression {}
| element_access {
  (* If only one expression, [element_access] can also be a
     [pointer_element_access]. *)
  }
| THIS {}
| base_access {}
| post_increment_expression {}
| post_decrement_expression {}
| object_creation_expression {
  (* If [NEW qualified_identifier LPAR expression RPAR],
     then it can be a [delegate_creation_expression]. *)
  } 
| typeof_expression {}
| CHECKED LPAR expression RPAR {}
| UNCHECKED LPAR expression RPAR {}
| pointer_member_access {}
| SIZEOF LPAR _type RPAR { (* [_type] is [unmanaged_type] *) }

predefined_type:
  BOOL {}
| BYTE {}
| CHAR {}
| DECIMAL {}
| DOUBLE {}
| FLOAT {}
| INT {}
| LONG {}
| OBJECT {}
| SBYTE {}
| SHORT {}
| STRING {}
| UINT {}
| ULONG {}
| USHORT {}

invocation_expression:
  primary_expression option(CAST) LPAR option(argument_list) RPAR {}

element_access:
  _primary_no_array_creation_expression
  LBRACKET expression_list RBRACKET {}
| qualified_identifier
  LBRACKET expression_list RBRACKET {}

expression_list:
  expression {}
| expression_list COMMA expression {}

base_access:
  BASE DOT Ident {}
| BASE LBRACKET expression_list RBRACKET {}

post_increment_expression:
  primary_expression DPLUS {}

post_decrement_expression:
  primary_expression DMINUS {}

object_creation_expression:
  NEW _type LPAR option(argument_list) RPAR {}

array_creation_expression:
  NEW non_array_type LBRACKET expression_list RBRACKET
    option(rank_specifiers) option(array_initializer) {}
| NEW array_type array_initializer {}

typeof_expression:
  TYPEOF ioption(CAST) LPAR _type RPAR {}
| TYPEOF LPAR VOID RPAR {}

unary_expression:
  primary_expression {}
| PLUS unary_expression {}
| MINUS unary_expression {}
| BANG unary_expression {}
| TILDE unary_expression {}
| pre_increment_expression {}
| pre_decrement_expression {}
| cast_expression {}
| pointer_indirection_expression {}
| addressof_expression {}

pointer_indirection_expression:
  TIMES unary_expression {}

pointer_member_access:
  primary_expression ARROW Ident {}

addressof_expression:
  AMPER unary_expression {}

pre_increment_expression:
  DPLUS unary_expression {}

pre_decrement_expression:
  DMINUS unary_expression {}

cast_expression:
  CAST LPAR _type RPAR unary_expression {}

multiplicative_expression:
  unary_expression {}
| multiplicative_expression TIMES unary_expression {}
| multiplicative_expression SLASH unary_expression {}
| multiplicative_expression PERCENT unary_expression {}

additive_expression:
  multiplicative_expression {}
| additive_expression PLUS multiplicative_expression {}
| additive_expression MINUS multiplicative_expression {}

shift_expression:
  additive_expression {}
| shift_expression LSHIFT additive_expression {}
| shift_expression RSHIFT additive_expression {}

relational_expression:
  shift_expression {}
| relational_expression LT shift_expression {}
| relational_expression GT shift_expression {}
| relational_expression LEQ shift_expression {}
| relational_expression GEQ shift_expression {}
| relational_expression IS _type {} (* No pointer *)
| relational_expression AS _type {} (* No pointer *)

equality_expression:
  relational_expression {}
| equality_expression EQ relational_expression {}
| equality_expression NE relational_expression {}

and_expression:
  equality_expression {}
| and_expression AMPER equality_expression {}

exclusive_or_expression:
  and_expression {}
| exclusive_or_expression CIRCUM and_expression {}

inclusive_or_expression:
  exclusive_or_expression {}
| inclusive_or_expression MID exclusive_or_expression {}

conditional_and_expression:
  inclusive_or_expression {}
| conditional_and_expression DAMPER inclusive_or_expression {}

conditional_or_expression:
  conditional_and_expression {}
| conditional_or_expression DMID conditional_and_expression {}

conditional_expression:
  conditional_or_expression {}
| conditional_or_expression QMARK expression COLON expression {}

assignment:
  unary_expression assignment_operator expression {}

assignment_operator:
  ASSIGN {}
| PLUSEQ {}
| MINUSEQ {}
| TIMESEQ {}
| DIVEQ   {}
| PEREQ   {}
| AMPEREQ {}
| MIDEQ   {}
| CIREQ    {}
| LSHIFTEQ {}
| RSHIFTEQ {}

expression:
  conditional_expression {}
| assignment {}

(* Statements *)

statement:
  Ident COLON statement { (* labeled_statement *) }
| local_variable_declaration SEMI { (* local variable decl *) }
| CONST _type constant_declarators SEMI { (* local constant decl *) }
| embedded_statement {}

embedded_statement:
  __open_embedded_statement {}
| __closed_embedded_statement {}

__open_embedded_statement:
  __open_if_statement {}
| __open_while_statement {}
| __open_for_statement {}
| __open_foreach_statement {}
| __open_lock_statement {} 
| __open_using_statement {} 
| __open_fixed_statement {} 

__closed_embedded_statement:
  __closed_if_statement {} 
| __closed_while_statement {} 
| __closed_for_statement {} 
| __closed_foreach_statement {} 
| __closed_lock_statement {} 
| __closed_using_statement {} 
| __closed_fixed_statement {} 
| __other_embedded_statement {}

__other_embedded_statement:
  block {} 
| SEMI {}
| statement_expression SEMI {}
| SWITCH LPAR expression RPAR switch_block {}
| DO embedded_statement WHILE LPAR expression RPAR SEMI {}
| BREAK SEMI {}
| CONTINUE SEMI {}
| GOTO Ident SEMI {}
| GOTO CASE expression SEMI {}
| GOTO DEFAULT SEMI {}
| RETURN option(expression) SEMI {}
| THROW option(expression) SEMI {}
| TRY block catch_clauses {}
| TRY block option(catch_clauses) FINALLY block {}
| CHECKED block {}
| UNCHECKED block {}
| UNSAFE block {}

__open_fixed_statement:
  FIXED LPAR pointer_type fixed_pointer_declarators RPAR
  __open_embedded_statement {}

__closed_fixed_statement:
  FIXED LPAR pointer_type fixed_pointer_declarators RPAR
  __closed_embedded_statement {}

fixed_pointer_declarators:
  fixed_pointer_declarator {}
| fixed_pointer_declarators COMMA fixed_pointer_declarator {}

fixed_pointer_declarator:
  Ident ASSIGN expression {}

block:
  LBRACE option(statement_list) RBRACE {}

statement_list:
  statement {}
| statement_list statement {}

local_variable_declaration:
  _type local_variable_declarators {}

local_variable_declarators:
  local_variable_declarator {}
| local_variable_declarators COMMA local_variable_declarator {}

local_variable_declarator:
  Ident {}
| Ident ASSIGN local_variable_initializer {}

local_variable_initializer:
  expression {}
| array_initializer {}
| STACKALLOC non_array_type LBRACKET expression RBRACKET {}

constant_declarators:
  constant_declarator {}
| constant_declarators COMMA constant_declarator {}

constant_declarator:
  Ident ASSIGN expression {}

statement_expression:
  invocation_expression {}
| object_creation_expression {}
| assignment {}
| post_increment_expression {}
| post_decrement_expression {}
| pre_increment_expression {}
| pre_decrement_expression {}

__open_if_statement:
  IF option(CAST) LPAR expression RPAR embedded_statement {}
| IF option(CAST) LPAR expression RPAR __closed_embedded_statement 
  ELSE __open_embedded_statement {}

__closed_if_statement:
  IF option(CAST) LPAR expression RPAR __closed_embedded_statement 
  ELSE __closed_embedded_statement {}

switch_block:
  LBRACE option(switch_sections) RBRACE {}

switch_sections:
  switch_section {}
| switch_sections switch_section {}

switch_section:
  switch_labels statement_list {}

switch_labels:
  switch_label {}
| switch_labels switch_label {}

switch_label:
  CASE expression COLON {}
| DEFAULT COLON {}

__open_while_statement:
  WHILE LPAR expression RPAR __open_embedded_statement {}

__closed_while_statement:
  WHILE LPAR expression RPAR __closed_embedded_statement {}

__open_for_statement:
  FOR LPAR ioption(for_initializer) SEMI
           ioption(expression) SEMI
           ioption(for_iterator) RPAR __open_embedded_statement {}

__closed_for_statement:
  FOR LPAR ioption(for_initializer) SEMI
           ioption(expression) SEMI
           ioption(for_iterator) RPAR __closed_embedded_statement {}

for_initializer:
  local_variable_declaration {}
| statement_expression_list {}

for_iterator:
  statement_expression_list {}

statement_expression_list:
  statement_expression {}
| statement_expression_list COMMA statement_expression {}

__open_foreach_statement:
  FOREACH LPAR _type Ident IN expression RPAR __open_embedded_statement {}

__closed_foreach_statement:
  FOREACH LPAR _type Ident IN expression RPAR __closed_embedded_statement {}

catch_clauses:
  general_catch_clause {}
| specific_catch_clauses option(general_catch_clause) {}

specific_catch_clauses:
  specific_catch_clause {}
| specific_catch_clauses specific_catch_clause {}

specific_catch_clause:
  CATCH LPAR class_type option(Ident) RPAR block {}

general_catch_clause:
  CATCH block {}

__open_lock_statement:
  LOCK LPAR expression RPAR __open_embedded_statement {}

__closed_lock_statement:
  LOCK LPAR expression RPAR __closed_embedded_statement {}

__open_using_statement:
  USING LPAR resource_acquisition RPAR __open_embedded_statement {}

__closed_using_statement:
  USING LPAR resource_acquisition RPAR __closed_embedded_statement {}

resource_acquisition:
  __local_variable_declaration {}
| expression {}

__local_variable_declaration:
  simple_type __local_variable_declarators {}
| class_type __local_variable_declarators {}

__local_variable_declarators:
  __local_variable_declarator {}
| __local_variable_declarators COMMA __local_variable_declarator {}

__local_variable_declarator:
  Ident ASSIGN local_variable_initializer {}

(* Universal modifiers *)

__modifiers:
  __modifier {}
| __modifiers __modifier {}

__modifier:
  NEW {}
| PUBLIC {}
| PROTECTED {}
| INTERNAL {}
| PRIVATE {}
| STATIC {}
| VIRTUAL {}
| SEALED {}
| OVERRIDE {}
| EXTERN {}
| READONLY {}
| VOLATILE {}
| ABSTRACT {}
| UNSAFE {}

(* Classes *)

class_declaration:
  ioption(attributes) ioption(__modifiers)
  CLASS Ident option(class_base)
  class_body option(SEMI) {
    (* [__modifiers] must be a list of unique tokens chosen among
       [ABSTRACT], [INTERNAL], [NEW], [PRIVATE], [PUBLIC], 
       [PROTECTED], [SEALED], [UNSAFE].
    *)
}

class_base:
  COLON class_type { 
    (* [qualified_identifier] is either a [class_type] or an 
       [interface_type] *)
  }
| COLON class_type COMMA interface_type_list {
    (* [qualified_identifier] is either a [class_type] or an 
       [interface_type] *)
  }

interface_type_list:
  qualified_identifier {}
| interface_type_list COMMA qualified_identifier {}

class_body:
  LBRACE option(class_member_declarations) RBRACE {}

class_member_declarations:
  class_member_declaration {}
| class_member_declarations class_member_declaration {}

class_member_declaration:
  struct_member_declaration {}
| destructor_declaration {}

(* In [constant_declaration], [__modifiers] must be a list of unique
   tokens among [INTERNAL], [NEW], [PRIVATE], [PROTECTED] and
   [PUBLIC]. *)

constant_declaration:
  ioption(attributes) ioption(__modifiers)
  CONST _type constant_declarators SEMI {}

(* We avoided here the redefinition of [constant_declarators] and
   [constant_declarator]. *)

field_declaration:
  ioption(attributes) ioption(__modifiers)
  _type variable_declarators SEMI {
    (* [__modifiers] must be a list of unique tokens among [INTERNAL],
       [NEW], [PRIVATE], [PUBLIC], [PROTECTED], [READONLY], [STATIC]
       and [VOLATILE]. *)
   }

variable_declarators:
  variable_declarator {}
| variable_declarators COMMA variable_declarator {}

variable_declarator:
  Ident {}
| Ident ASSIGN variable_initializer {}

variable_initializer:
  expression {}
| array_initializer {}

(* [__modifiers] must be a list of unique tokens among [ABSTRACT],
   [EXTERN], [INTERNAL], [NEW] [OVERRIDE], [PRIVATE], [PROTECTED],
   [PUBLIC], [SEALED], [STATIC], [UNSAFE] and [VIRTUAL]. *)

method_header:
  ioption(attributes) ioption(__modifiers) _type
  qualified_identifier
  LPAR option(formal_parameter_list) RPAR {}
| ioption(attributes) ioption(__modifiers) VOID
  qualified_identifier
  LPAR option(formal_parameter_list) RPAR {}

__common_body:
  block {}
| SEMI {}

formal_parameter_list:
  fixed_parameters {}
| fixed_parameters COMMA parameter_array {}
| parameter_array {}

fixed_parameters:
  fixed_parameter {}
| fixed_parameters COMMA fixed_parameter {}

fixed_parameter:
  option(attributes) option(parameter_modifier) _type Ident {}

parameter_modifier:
  REF {}
| OUT {}

parameter_array:
  option(attributes) PARAMS array_type Ident {}

property_declaration:
  ioption(attributes) ioption(__modifiers) _type
  qualified_identifier LBRACE accessor_declarations RBRACE {
  (* [__modifiers] must be a list of unique tokens among [ABSTRACT],
     [EXTERN], [INTERNAL], [NEW], [OVERRIDE], [PROTECTED], [PRIVATE],
     [PUBLIC], [SEALED], [STATIC], [UNSAFE] and [VIRTUAL]. 
     [accessor_declarations] must be "get/set" or "set/get".
  *)
  }

(* We avoided here the redefinition of [member_name], which was later
   on inlined into the new rule [qualified_identifier]. *)

accessor_declarations:
  accessor_declaration option(accessor_declaration) {}

accessor_declaration:
  option(attributes) Ident __common_body {}

(* In [event_declaration], [__modifiers] must be a list of unique
   tokens among [ABSTRACT], [EXTERN], [INTERNAL], [NEW], [OVERRIDE],
   [PRIVATE], [PROTECTED], [PUBLIC], [SEALED], [STATIC], [UNSAFE] and
   [VIRTUAL]. *)

event_declaration:
  ioption(attributes) ioption(__modifiers)
  EVENT _type variable_declarators SEMI {}
| ioption(attributes) ioption(__modifiers)
  EVENT _type qualified_identifier
  LBRACE accessor_declarations RBRACE {
    (* [accessor_declarations] must be "add/remove" or "remove/add". *)
  }

indexer_declaration:
  ioption(attributes) ioption(__modifiers)
  indexer_declarator LBRACE accessor_declarations RBRACE {
  (* [__modifiers] must be a list of unique tokens among [ABSTRACT],
     [EXTERN], [INTERNAL], [NEW], [OVERRIDE], [PRIVATE], [PROTECTED],
     [PUBLIC], [SEALED], [UNSAFE], [VIRTUAL]. *)
  }

indexer_declarator:
  _type THIS LBRACKET formal_parameter_list RBRACKET {}
| _type qualified_identifier DOT THIS 
  LBRACKET formal_parameter_list RBRACKET {}

operator_declaration:
  ioption(attributes) __modifiers
  operator_declarator __common_body {
   (* [__modifiers] must be a list of unique tokens among [EXTERN],
      [PUBLIC], [STATIC] and [UNSAFE]. *)
  }

operator_declarator:
  unary_operator_declarator  {}
| binary_operator_declarator {}
| conversion_operator_declarator {}

unary_operator_declarator:
  _type OPERATOR PLUS LPAR _type Ident RPAR {}
| _type OPERATOR MINUS LPAR _type Ident RPAR {}
| _type OPERATOR overloadable_unary_operator
  LPAR _type Ident RPAR {}

overloadable_unary_operator:
  BANG {}
| TILDE {}
| DPLUS {}
| DMINUS {}
| TRUE {}
| FALSE {}

binary_operator_declarator:
  _type OPERATOR PLUS
  LPAR _type Ident COMMA _type Ident RPAR {}
| _type OPERATOR MINUS
  LPAR _type Ident COMMA _type Ident RPAR {}
| _type OPERATOR overloadable_binary_operator
  LPAR _type Ident COMMA _type Ident RPAR {}

overloadable_binary_operator:
  TIMES {}
| SLASH {}
| PERCENT {}
| AMPER {}
| MID {}
| CIRCUM {}
| LSHIFT {}
| RSHIFT {}
| EQ {}
| NE {}
| GT {}
| LT {}
| GEQ {}
| LEQ {}

conversion_operator_declarator:
  IMPLICIT OPERATOR _type LPAR _type Ident RPAR {}
| EXPLICIT OPERATOR _type LPAR _type Ident RPAR {}

constructor_declaration:
  ioption(attributes) ioption(__modifiers)
  constructor_declarator __common_body {
    (* [__modifiers] must be a list of unique tokens among [INTERNAL],
       [EXTERN], [PRIVATE], [PROTECTED], [PUBLIC] and [UNSAFE]. *)
  }
| ioption(attributes)
  Ident LPAR RPAR __common_body {}
| ioption(attributes) __modifiers
  Ident LPAR RPAR __common_body {
    (* If [__modifiers] does not contain [STATIC], then it must be a
       list of unique tokens among [INTERNAL], [EXTERN], [PRIVATE],
       [PROTECTED], [PUBLIC] and [UNSAFE]; otherwise a static
       constructor is being declared and [__modifiers] must one the
       following lists of tokens: [[STATIC;EXTERN]],
       [[STATIC;EXTERN;UNSAFE], [[STATIC;UNSAFE]],
       [[STATIC;UNSAFE;EXTERN]], [[EXTERN;STATIC]],
       [[EXTERN;STATIC;UNSAFE]], [[EXTERN;UNSAFE;STATIC],
       [[UNSAFE;STATIC]], [[UNSAFE;STATIC;EXTERN]] and
       [[UNSAFE;EXTERN;STATIC]]. *)
}

constructor_declarator:
  Ident LPAR formal_parameter_list RPAR
  option(constructor_initializer) {}
| Ident LPAR RPAR constructor_initializer {}

constructor_initializer:
  COLON BASE LPAR option(argument_list) RPAR {}
| COLON THIS LPAR option(argument_list) RPAR {}

(* [__modifiers] must be one of [[EXTERN]], [[EXTERN;UNSAFE],
   [[UNSAFE]], [[UNSAFE; EXTERN]]. *)

destructor_declaration:
  TILDE Ident LPAR RPAR __common_body {}
| attributes TILDE Ident LPAR RPAR __common_body {}
| attributes __modifiers TILDE Ident LPAR RPAR __common_body {}
| __modifiers TILDE Ident LPAR RPAR __common_body {}

(* Structs *)

struct_declaration:
  ioption(attributes) ioption(__modifiers)
  STRUCT Ident
  option(struct_interfaces) struct_body option(SEMI) {
   (* [__modifiers] must be a list of unique tokens among [INTERNAL],
      [NEW], [PRIVATE], [PROTECTED], [PUBLIC] and [UNSAFE]. *)
  }

struct_interfaces:
  COLON interface_type_list {}

struct_body:
  LBRACE option(struct_member_declarations) RBRACE {}

struct_member_declarations:
  struct_member_declaration {}
| struct_member_declarations struct_member_declaration {}

struct_member_declaration:
  constant_declaration {}
| field_declaration {}
| method_header __common_body { (* method_declaration *) }
| property_declaration {}
| event_declaration {}
| indexer_declaration {}
| operator_declaration {}
| constructor_declaration {}
| type_declaration {}

(* Arrays *)

(* We avoided here the redefinition of [array_type], [non_array_type],
   [rank_specifiers], [rank_specifier] and [dim_separators]. *)

array_initializer:
  LBRACE option(variable_initializer_list) RBRACE {}
| LBRACE variable_initializer_list COMMA RBRACE {}

variable_initializer_list:
  variable_initializer {}
| variable_initializer_list COMMA variable_initializer {}

(* We avoided here the redefinition of [variable_initializer]. *)

(* Interfaces *)

interface_declaration:
  ioption(attributes) ioption(__modifiers)
  INTERFACE Ident 
  option(interface_base) interface_body option(SEMI) {
    (* [__modifiers] must be a list of unique tokens among [INTERNAL],
       [NEW], [PRIVATE], [PROTECTED], [PUBLIC] and [UNSAFE]. *)
  }

interface_base:
  COLON interface_type_list {}

interface_body:
  LBRACE option(interface_member_declarations) RBRACE {}

interface_member_declarations:
  interface_member_declaration {}
| interface_member_declarations interface_member_declaration {}

interface_member_declaration:
  interface_method_declaration {}
| interface_property_declaration {}
| interface_event_declaration {}
| interface_indexer_declaration {}

interface_method_declaration:
  ioption(attributes) option(NEW) _type Ident
  LPAR option(formal_parameter_list) RPAR SEMI {}
| ioption(attributes) option(NEW) VOID Ident
  LPAR option(formal_parameter_list) RPAR SEMI {}

interface_property_declaration:
  ioption(attributes) option(NEW) _type Ident
  LBRACE interface_accessors RBRACE {}

interface_accessors:
  ioption(attributes) Ident SEMI { (* Ident="get" or Ident="set" *) }
| ioption(attributes) Ident SEMI ioption(attributes) Ident SEMI {
  (* Ident1="set" and Ident2="get", or Ident1="get" and Ident2="set" *)
  }

interface_event_declaration:
  ioption(attributes) option(NEW) EVENT _type Ident SEMI {}

interface_indexer_declaration:
  ioption(attributes) option(NEW) __alt_type THIS
  LBRACKET formal_parameter_list RBRACKET
  LBRACE interface_accessors RBRACE {}

%inline __alt_type:
  non_array_type {
   (* In [class_type], [qualified_identifier] can be a
      [delegate_type], an [qualified_identifier], an [enum_type] or a
      [struct_type]. *) }
| array_type {} 

(* Enums *)

enum_declaration:
  ioption(attributes) ioption(__modifiers) ENUM Ident
  option(enum_base) enum_body option(SEMI) {
    (* [__modifiers] must be a list of unique tokens among [INTERNAL],
       [NEW], [PRIVATE], [PROTECTED] and [PUBLIC]. *)
  }

enum_base:
  COLON integral_type {}

enum_body:
  LBRACE option(enum_member_declarations) RBRACE {}
| LBRACE enum_member_declarations COMMA RBRACE {}

enum_member_declarations:
  enum_member_declaration {}
| enum_member_declarations COMMA enum_member_declaration {}

enum_member_declaration:
  ioption(attributes) Ident {}
| ioption(attributes) Ident ASSIGN expression {}

(* Delegates *)

(* [__modifiers] must be a list of unique tokens among [INTERNAL],
   [NEW], [PRIVATE], [PROTECTED], [PUBLIC] and [UNSAFE]. *)

delegate_declaration:
  ioption(attributes) ioption(__modifiers) DELEGATE _type
  Ident LPAR option(formal_parameter_list) RPAR SEMI {}
| ioption(attributes) ioption(__modifiers) DELEGATE VOID
  Ident LPAR option(formal_parameter_list) RPAR SEMI {}

(* Attributes *)

attributes:
  attribute_section {}
| attributes attribute_section {}

attribute_section:
  LBRACKET ioption(attribute_target_specifier)
  attribute_list option(COMMA) RBRACKET {}

attribute_target_specifier:
  attribute_target COLON {}

attribute_target:
  Ident { 
    (* Ident="field", "method", "param", "property", "type".
       If a global attribute: Ident="assembly", "module". *) 
  }
| EVENT {}
| RETURN {}

attribute_list:
  attribute {}
| attribute_list COMMA attribute {}

attribute:
  qualified_identifier option(attribute_arguments) {}

attribute_arguments:
  LPAR option(expression_list) RPAR {
    (* If there is a prefix of the [positional_argument_list] which
       matches "Ident ASSIGN expression", it can be interpreted as a
       [named_argument_list]. *)
  }
