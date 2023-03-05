%{
(* Menhir specification for C#
   ECMA-334, Annex A, page 443 (evince: p.465) 

   No preprocessor directives.
   No unsafe code.
   Disabled global attributes.
*)
%}

(* Keywords *)

%token ABSTRACT AS       BASE        BOOL     BREAK
%token BYTE     CASE     CATCH      CHAR      CHECKED
%token CLASS    CONST    CONTINUE   DECIMAL   DEFAULT
%token DELEGATE DO       DOUBLE     ELSE      ENUM
%token EVENT    EXPLICIT EXTERN     FALSE     FINALLY
%token          FLOAT    FOR        FOREACH   GOTO
%token IF       IMPLICIT IN         INT       INTERFACE
%token INTERNAL IS       LOCK       LONG      NAMESPACE
%token NEW      NULL     OBJECT     OPERATOR  OUT
%token OVERRIDE PARAMS   PARTIAL    PRIVATE   PROTECTED PUBLIC
%token READONLY REF      RETURN     SBYTE     SEALED
%token SHORT                        STATIC    STRING
%token STRUCT   SWITCH   THIS       THROW     TRUE
%token TRY      TYPEOF   UINT       ULONG     UNCHECKED
%token          USHORT   USING      VIRTUAL   VOID
%token VOLATILE WHILE    WHERE      YIELD

(* 
%token FIXED SIZEOF STACKALLOC UNSAFE
*)
(* Virtual keywords *)

%token NOSPACE

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
%token DQMARK    (* ??  *)
%token DCOLON    (* ::  *)
%token DPLUS     (* ++  *)
%token DMINUS    (* --  *)
%token DAMPER    (* &&  *)
%token DMID      (* ||  *)

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
%token LSHIFT    (* <<  *)
%token LSHIFTEQ  (* <<= *)

(* Literals which are not keywords *)

%token <Lexis.text_kind * string> Ident
%token <Lexis.text_kind * string> String
%token <string> Int
%token <string> Float
%token <string> Char

(* End-Of-File sentinel *)

%token EOF

(* Entries *)

%start main
%type <unit> main

%%

(* Grammar *)

main:
  compilation_unit EOF {}

(* Basic concepts (A.2.1, page 450, evince: p.472) *)

compilation_unit:
  ioption(extern_alias_directives)
  ioption(using_directives)
(*  ioption(global_attributes) *)
  ioption(namespace_member_declarations) {}

namespace_name:
  namespace_or_type_name {}

type_name:
  namespace_or_type_name {}

namespace_or_type_name:
  Ident ioption(type_argument_list) {}
| qualified_alias_member {}
| namespace_or_type_name DOT Ident ioption(type_argument_list) {}

(* Types (A.2.2, page 451, evince: p.473 *)

_type:
  value_type {} (* May be [type_parameter] *)
| reference_type {}
(*| type_parameter {} *)

value_type:
  struct_type { (* [type_name] may be [struct_type] or 
                   [enum_type]. *) }
struct_type:
  type_name {}
| simple_type {}
| nullable_type {}

simple_type:
  numeric_type {}
| BOOL {}

numeric_type:
  integral_type {}
| floating_point_type {}
| DECIMAL {}

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

floating_point_type:
  FLOAT {}
| DOUBLE {}

nullable_type:
  type_name (* May be [enum_type]. *) QMARK {}
| simple_type QMARK {}

reference_type:
  class_type { (* [type_name] may be [class_type],
                  [interface_type] or [delegate_type]. *) 
    }
| array_type {}

class_type:
  type_name {}
| OBJECT {}
| STRING {}

%inline interface_type:
  type_name {}

(* Here, duplicate of rule [array_type] until [dim_separators].
   See below. *)

(* Variables (A.2.3, page 452, evince: p.474) *)

variable_reference:
  expression {}

(* Expressions (A.2.3, page 452, evince: p.474) *)

argument_list:
  argument {}
| argument_list COMMA argument {}

argument:
  expression {}
| REF variable_reference {}
| OUT variable_reference {}

primary_expression:
  array_creation_expression {}
| primary_no_array_creation_expression {}

primary_no_array_creation_expression:
  literal {}
| simple_name {}
| parenthesized_expression {}
| member_access {}
| invocation_expression {}
| element_access {}
| this_access {}
| base_access {}
| post_increment_expression {}
| post_decrement_expression {}
| object_creation_expression {}
| delegate_creation_expression {}
| typeof_expression {}
| checked_expression {}
| unchecked_expression {}
| default_value_expression {}
| anonymous_method_expression {}

literal:
  boolean_literal {}
| integer_literal {}
| real_literal {}
| character_literal {}
| string_literal {}
| null_literal {}

boolean_literal:
  TRUE {}
| FALSE {}

integer_literal:
  Int {}

real_literal:
  Float {}

character_literal:
  Char {}

string_literal:
  String {}

null_literal:
  NULL {}

(* Resuming previous section now... *)

simple_name:
  Ident ioption(type_argument_list) {}

parenthesized_expression:
  LPAR expression RPAR {}

member_access:
  primary_expression DOT Ident ioption(type_argument_list) {}
| predefined_type DOT Ident ioption(type_argument_list) {}
| qualified_alias_member DOT Ident ioption(type_argument_list) {}

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
  primary_expression LPAR ioption(argument_list) RPAR {}

element_access:
  primary_no_array_creation_expression
  LBRACKET expression_list RBRACKET {}

expression_list:
  expression {}
| expression_list COMMA expression {}

this_access:
  THIS {}

base_access:
  BASE DOT Ident ioption(type_argument_list) {}
| BASE LBRACKET expression_list RBRACKET {}

post_increment_expression:
  primary_expression DPLUS {}

post_decrement_expression:
  primary_expression DMINUS {}

object_creation_expression:
  NEW _type LPAR ioption(argument_list) RPAR {}

array_creation_expression:
  NEW non_array_type LBRACKET expression_list RBRACKET
    ioption(rank_specifiers) ioption(array_initializer) {}
| NEW array_type array_initializer {}

delegate_creation_expression:
  NEW type_name LPAR expression RPAR {}

typeof_expression:
  TYPEOF LPAR _type RPAR {}
| TYPEOF LPAR _unbound_type_name RPAR {}
| TYPEOF LPAR VOID RPAR {}

_unbound_type_name:
  Ident ioption(generic_dimension_specifier) _x {}
| Ident DCOLON Ident ioption(generic_dimension_specifier) _x {}

_x:
  DOT Ident ioption(generic_dimension_specifier) _x {}
| (* empty *) {}

generic_dimension_specifier:
  LT ioption(commas) _gt {}

_gt:
  GT ioption(NOSPACE) {}

commas:
  COMMA {}
| commas COMMA {}

checked_expression:
  CHECKED LPAR expression RPAR {}

unchecked_expression:
  UNCHECKED LPAR expression RPAR {}

default_value_expression:
  DEFAULT LPAR _type RPAR {}

anonymous_method_expression:
  DELEGATE ioption(anonymous_method_signature) block {}

anonymous_method_signature:
  LPAR ioption(anonymous_method_parameter_list) RPAR {}

anonymous_method_parameter_list:
  anonymous_method_parameter {}
| anonymous_method_parameter_list COMMA anonymous_method_parameter {}

anonymous_method_parameter:
  ioption(parameter_modifier) _type Ident {}

unary_expression:
  primary_expression {}
| PLUS unary_expression {}
| MINUS unary_expression {}
| BANG unary_expression {}
| TILDE unary_expression {}
| pre_increment_expression {}
| pre_decrement_expression {}
| cast_expression {}
 
pre_increment_expression:
  DPLUS unary_expression {}

pre_decrement_expression:
  DMINUS unary_expression {}

cast_expression:
  LPAR _type RPAR unary_expression {}

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
| shift_expression right_shift additive_expression {}

right_shift:
  GT NOSPACE _gt {}

relational_expression:
  shift_expression {}
| relational_expression LT shift_expression {}
| relational_expression _gt shift_expression {}
| relational_expression LEQ shift_expression {}
| relational_expression GEQ shift_expression {}
| relational_expression IS _type {}
| relational_expression AS _type {}

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

null_coalescing_expression:
  conditional_or_expression {}
| conditional_or_expression DQMARK null_coalescing_expression {}

conditional_expression:
  null_coalescing_expression {}
| null_coalescing_expression QMARK expression COLON expression {}

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
| LSHIFTEQ  {}
| right_shift_assignment {}

right_shift_assignment:
  GT NOSPACE GT NOSPACE ASSIGN {}

expression:
  conditional_expression {}
| assignment {}

constant_expression:
  expression {}

boolean_expression:
  expression {}

(* Statements (A.2.5, page 456, evince: p.478) *)

statement:
  labeled_statement {}
| declaration_statement {}
| embedded_statement {}

embedded_statement:
  block {}
| empty_statement {}
| expression_statement {}
| selection_statement {}
| iteration_statement {}
| jump_statement {}
| try_statement {}
| checked_statement {}
| unchecked_statement {}
| lock_statement {}
| using_statement {}
| yield_statement {}

block:
  LBRACE ioption(statement_list) RBRACE {}

statement_list:
  statement {}
| statement_list statement {}

empty_statement:
  SEMI {}

labeled_statement:
  Ident COLON statement {}

declaration_statement:
  local_variable_declaration SEMI {}
| local_constant_declaration SEMI {}

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

local_constant_declaration:
  CONST _type constant_declarators {}

constant_declarators:
  constant_declarator {}
| constant_declarators COMMA constant_declarator {}

constant_declarator:
  Ident ASSIGN constant_expression {}

expression_statement:
  statement_expression SEMI {}

statement_expression:
  invocation_expression {}
| object_creation_expression {}
| assignment {}
| post_increment_expression {}
| post_decrement_expression {}
| pre_increment_expression {}
| pre_decrement_expression {}

selection_statement:
  if_statement {}
| switch_statement {}

if_statement:
  IF LPAR boolean_expression RPAR embedded_statement {}
| IF LPAR boolean_expression RPAR embedded_statement 
  ELSE embedded_statement {}

switch_statement:
  SWITCH LPAR expression RPAR switch_block {}

switch_block:
  LBRACE ioption(switch_sections) RBRACE {}

switch_sections:
  switch_section {}
| switch_sections switch_section {}

switch_section:
  switch_labels statement_list {}

switch_labels:
  switch_label {}
| switch_labels switch_label {}

switch_label:
  CASE constant_expression COLON {}
| DEFAULT COLON {}

iteration_statement:
  while_statement {}
| do_statement {}
| for_statement {}
| foreach_statement {}

while_statement:
  WHILE LPAR boolean_expression RPAR embedded_statement {}

do_statement:
  DO embedded_statement WHILE LPAR boolean_expression RPAR SEMI {}

for_statement:
  FOR LPAR ioption(for_initializer) SEMI
           ioption(for_condition) SEMI
           ioption(for_iterator) RPAR embedded_statement {}

for_initializer:
  local_variable_declaration {}
| statement_expression_list {}

for_condition:
  boolean_expression {}

for_iterator:
  statement_expression_list {}

statement_expression_list:
  statement_expression {}
| statement_expression_list COMMA statement_expression {}

foreach_statement:
  FOREACH LPAR _type Ident IN expression RPAR embedded_statement {}

jump_statement:
  break_statement {}
| continue_statement {}
| goto_statement {}
| return_statement {}
| throw_statement {}

break_statement:
  BREAK SEMI {}

continue_statement:
  CONTINUE SEMI {}

goto_statement:
  GOTO Ident SEMI {}
| GOTO CASE constant_expression SEMI {}
| GOTO DEFAULT SEMI {}

return_statement:
  RETURN ioption(expression) SEMI {}

throw_statement:
  THROW ioption(expression) SEMI {}

try_statement:
  TRY block catch_clauses {}
| TRY block ioption(catch_clauses) finally_clause {}

catch_clauses:
  specific_catch_clauses {}
| ioption(specific_catch_clauses) general_catch_clause {}

specific_catch_clauses:
  specific_catch_clause {}
| specific_catch_clauses specific_catch_clause {}

specific_catch_clause:
  CATCH LPAR class_type ioption(Ident) RPAR block {}

general_catch_clause:
  CATCH block {}

finally_clause:
  FINALLY block {}

checked_statement:
  CHECKED block {}

unchecked_statement:
  UNCHECKED block {}

lock_statement:
  LOCK LPAR expression RPAR embedded_statement {}

using_statement:
  USING LPAR resource_acquisition RPAR embedded_statement {}

resource_acquisition:
  local_variable_declaration {}
| expression {}

yield_statement:
  YIELD RETURN expression SEMI {}
| YIELD BREAK SEMI {}

namespace_declaration:
  NAMESPACE qualified_identifier namespace_body ioption(SEMI) {}

qualified_identifier:
  Ident {}
| qualified_identifier DOT Ident {}

namespace_body:
  LBRACE ioption(extern_alias_directives)
         ioption(using_directives)
         ioption(namespace_member_declarations) RBRACE {}

extern_alias_directives:
  extern_alias_directive {}
| extern_alias_directives extern_alias_directive {}

extern_alias_directive:
  EXTERN Ident Ident SEMI {
  (* Ident1 = "alias" *)
}

using_directives:
  using_directive {}
| using_directives using_directive {}

using_directive:
  using_alias_directive {}
| using_namespace_directive {}

using_alias_directive:
  USING Ident ASSIGN namespace_or_type_name SEMI {}

using_namespace_directive:
  USING namespace_name SEMI {}

namespace_member_declarations:
  namespace_member_declaration {}
| namespace_member_declarations namespace_member_declaration {}

namespace_member_declaration:
  namespace_declaration {}
| type_declaration {}

type_declaration:
  class_declaration {}
| struct_declaration {}
| interface_declaration {}
| enum_declaration {}
| delegate_declaration {}

qualified_alias_member:
  Ident DCOLON Ident ioption(type_argument_list) {}

(* Classes (A.2.6, page 460, evince: p.482) *)

class_declaration:
  ioption(attributes) ioption(_class_modifiers) ioption(PARTIAL)
  CLASS Ident ioption(type_parameter_list) ioption(class_base)
  ioption(type_parameter_constraints_clauses)
  class_body ioption(SEMI) {}

_class_modifiers:
  _class_access _class_kind {}
| _class_kind _class_access {}
| NEW _class_access _class_kind {}
| NEW _class_kind _class_access {}
| _class_access NEW _class_kind {}
| _class_kind NEW _class_access {}
| _class_access _class_kind NEW {}
| _class_kind _class_access NEW {}

%inline _class_access:
  PUBLIC {}
| PROTECTED {}
| INTERNAL {}
| PROTECTED INTERNAL {}
| PRIVATE {}

%inline _class_kind:
  SEALED {}
| STATIC {}
| ABSTRACT {}

class_base:
  COLON interface_type {}   (* [interface_type] may be a [class_type] *)
| COLON type_name COMMA interface_type_list {}
| COLON OBJECT COMMA interface_type_list {}
| COLON STRING COMMA interface_type_list {}

interface_type_list:
  interface_type {}
| interface_type_list COMMA interface_type {}

class_body:
  LBRACE ioption(class_member_declarations) RBRACE {}

class_member_declarations:
  class_member_declaration {}
| class_member_declarations class_member_declaration {}

class_member_declaration:
  constant_declaration {}
| field_declaration {}
| method_declaration {}
| property_declaration {}
| event_declaration {}
| indexer_declaration {}
| operator_declaration {}
| constructor_declaration {}
| finalizer_declaration {}
| static_constructor_declaration {}
| type_declaration {}

constant_declaration:
  ioption(attributes) ioption(_constant_modifiers) 
  CONST _type constant_declarators SEMI {}

_constant_modifiers:
  NEW _class_access {}
| _class_access NEW {}
| _class_access {}

(* Here, omission of [constant_declarators] and [constant_declarator].
  See above. *)

field_declaration:
  ioption(attributes) ioption(_field_modifiers)
  _type variable_declarators SEMI {}

%inline _field_modifiers:
  _class_access STATIC {}
| _class_access READONLY {}
| _class_access VOLATILE {}
| _field_kind _class_access {}
| NEW _class_access _field_kind {}
| NEW _field_kind _class_access {}
| _class_access NEW _field_kind {}
| _field_kind NEW _class_access {}
| _class_access _field_kind NEW {}
| _field_kind _class_access NEW {}

%inline _field_kind:
  STATIC {}
| READONLY {}
| VOLATILE {}

variable_declarators:
  variable_declarator {}
| variable_declarators COMMA variable_declarator {}

variable_declarator:
  Ident {}
| Ident ASSIGN variable_initializer {}

variable_initializer:
  expression {}
| array_initializer {}

method_declaration:
  method_header method_body {}

method_header:
  ioption(attributes) ioption(_method_modifiers) return_type
  member_name ioption(type_parameter_list)
  LPAR ioption(formal_parameter_list) RPAR
  ioption(type_parameter_constraints_clauses) {}

%inline _method_modifiers: (* Except EXTERN *)
  _class_access _method_kind {} 
| _class_access _method_kind NEW {}

(*
  _class_access SEALED OVERRIDE {}
| _class_access STATIC {}
| _class_access ABSTRACT ioption(OVERRIDE) {}
| _class_access VIRTUAL {}
| _class_access OVERRIDE ABSTRACT SEALED {}
| _class_access OVERRIDE SEALED ioption(ABSTRACT) {}
| _class_access EXTERN {}

| _class_access SEALED OVERRIDE NEW {}
| _class_access STATIC NEW {}
| _class_access ABSTRACT ioption(OVERRIDE) NEW {}
| _class_access VIRTUAL NEW {}
| _class_access OVERRIDE ABSTRACT SEALED NEW {}
| _class_access OVERRIDE SEALED ioption(ABSTRACT) NEW {}
| _class_access EXTERN NEW {}
*)

| _method_kind _class_access {}
| NEW _class_access _method_kind {}
| NEW _method_kind _class_access {}
| _class_access NEW _method_kind {}
| _method_kind NEW _class_access {}
| _method_kind _class_access NEW {}

%inline _method_kind:
  SEALED OVERRIDE {}
| STATIC {}
| ABSTRACT ioption(OVERRIDE) {}
| VIRTUAL {}
| OVERRIDE ABSTRACT SEALED {}
| OVERRIDE SEALED ioption(ABSTRACT) {}
| EXTERN {}

%inline return_type:
  _type {}
| VOID {}

%inline member_name:
  Ident {}
| interface_type DOT Ident {}

method_body:
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
  ioption(attributes) ioption(parameter_modifier) _type Ident {}

parameter_modifier:
  REF {}
| OUT {}

parameter_array:
  ioption(attributes) PARAMS array_type Ident {}

property_declaration:
  ioption(attributes) ioption(_method_modifiers) _type
  member_name LBRACE accessor_declarations RBRACE {}

accessor_declarations:
  get_accessor_declaration ioption(set_accessor_declaration) {}
| set_accessor_declaration ioption(get_accessor_declaration) {}

get_accessor_declaration:
  ioption(attributes) ioption(accessor_modifier)
  Ident accessor_body { (* Ident="get" *) }

set_accessor_declaration:
  ioption(attributes) ioption(accessor_modifier)
  Ident accessor_body { (* Ident="set" *) }

accessor_modifier:
  PROTECTED {}
| INTERNAL {}
| PRIVATE {}
| PROTECTED INTERNAL {}
| INTERNAL PROTECTED {}

accessor_body:
  block {}
| SEMI {}

event_declaration:
  ioption(attributes) ioption(_event_modifiers)
  EVENT _type variable_declarators SEMI {}
| ioption(attributes) ioption(_event_modifiers)
  EVENT _type member_name
  LBRACE event_accessor_declarations RBRACE {}

_event_modifiers:
  _class_modifiers {}

event_accessor_declarations:
  add_accessor_declaration remove_accessor_declaration {}
| remove_accessor_declaration add_accessor_declaration {}

add_accessor_declaration:
  ioption(attributes) Ident block {} (* Ident="add" *)

remove_accessor_declaration:
  ioption(attributes) Ident block {} (* Ident="remove" *)

indexer_declaration: (* Except STATIC *)
  ioption(attributes) ioption(_method_modifiers)
  indexer_declarator LBRACE accessor_declarations RBRACE {}

indexer_declarator:
  _type THIS LBRACKET formal_parameter_list RBRACKET {}
| _type interface_type DOT THIS 
  LBRACKET formal_parameter_list RBRACKET {}

operator_declaration:
  ioption(attributes) _operator_modifiers
  operator_declarator operator_body {}

%inline _operator_modifiers:
  PUBLIC {}
| PUBLIC STATIC {}
| STATIC {}
| STATIC PUBLIC {}
| EXTERN {}

operator_declarator:
  _type OPERATOR overloadable_unary_operator
  LPAR _type Ident RPAR { (* [unary_operator_declarator] *) }
| _type OPERATOR overloadable_binary_operator
  LPAR _type Ident COMMA _type Ident RPAR
  { (* [binary_operator_declarator] *) }
| conversion_operator_declarator {}

%inline overloadable_unary_operator:
  PLUS {}
| MINUS {}
| BANG {}
| TILDE {}
| DPLUS {}
| DMINUS {}
| TRUE {}
| FALSE {}

%inline overloadable_binary_operator:
  PLUS {}
| MINUS {}
| TIMES {}
| SLASH {}
| PERCENT {}
| AMPER {}
| MID {}
| CIRCUM {}
| LSHIFT {}
| right_shift {}
| EQ {}
| NE {}
| _gt {}
| LT {}
| _geq {}
| LEQ {}

_geq:
  GT NOSPACE ASSIGN {}

conversion_operator_declarator:
  IMPLICIT OPERATOR _type LPAR _type Ident RPAR {}
| EXPLICIT OPERATOR _type LPAR _type Ident RPAR {}

operator_body:
  block {}
| SEMI {}

constructor_declaration:
  ioption(attributes) ioption(_constructor_modifiers)
  constructor_declarator constructor_body {}

%inline _constructor_modifiers:
  PUBLIC {}
| PROTECTED {}
| PROTECTED INTERNAL {}
| INTERNAL {}
| PRIVATE {}
| EXTERN {}

constructor_declarator:
  Ident LPAR ioption(formal_parameter_list) RPAR
  ioption(constructor_initializer) {}

constructor_initializer:
  COLON BASE LPAR ioption(argument_list) RPAR {}
| COLON THIS LPAR ioption(argument_list) RPAR {}

constructor_body:
  block {}
| SEMI {}

static_constructor_declaration:
  ioption(attributes) 
  STATIC
  Ident LPAR RPAR static_constructor_body {}
| ioption(attributes) 
  EXTERN STATIC
  Ident LPAR RPAR static_constructor_body {}
| ioption(attributes) 
  STATIC EXTERN
  Ident LPAR RPAR static_constructor_body {}

static_constructor_body:
  block {}
| SEMI {}

finalizer_declaration:
  ioption(attributes) ioption(EXTERN)
  TILDE Ident LPAR RPAR finalizer_body {}

finalizer_body:
  block {}
| SEMI {}

(* Structs (A.2.7, page 465, evince: p.487) *)

struct_declaration:
  ioption(attributes) ioption(_struct_modifiers) ioption(PARTIAL)
  STRUCT Ident ioption(type_parameter_list)
  ioption(struct_interfaces) ioption(type_parameter_constraints_clauses)
  struct_body ioption(SEMI) {}

%inline _struct_modifiers:
  _interface_modifiers {}

struct_interfaces:
  COLON interface_type_list {}

struct_body:
  LBRACE ioption(struct_member_declarations) RBRACE {}

struct_member_declarations:
  struct_member_declaration {}
| struct_member_declarations struct_member_declaration {}

struct_member_declaration:
  constant_declaration {}
| field_declaration {}
| method_declaration {}
| property_declaration {}
| event_declaration {}
| indexer_declaration {}
| operator_declaration {}
| constructor_declaration {}
| static_constructor_declaration {}
| type_declaration {}

(* Arrays (A.2.8, page 466, evince: p.488) *)

array_type:
  non_array_type rank_specifiers {}

non_array_type:
  value_type {} (* May be [type_parameter] *)
| class_type { (* [type_name] may be [class_type],
                  [interface_type] or [delegate_type]. *) }
(* | type_parameter {} *)

rank_specifiers:
  rank_specifier {}
| rank_specifiers rank_specifier {}

rank_specifier:
  LBRACKET ioption(dim_separators) RBRACKET {}

dim_separators:
  COMMA {}
| dim_separators COMMA {}

array_initializer:
  LBRACE ioption(variable_initializer_list) RBRACE {}
| LBRACE variable_initializer_list COMMA RBRACE {}

variable_initializer_list:
  variable_initializer {}
| variable_initializer_list COMMA variable_initializer {}

(* Here, omission of [variable_initializer]. See above. *)

(* Interfaces (A.2.9, page 467, evince: p.489) *)

interface_declaration:
  ioption(attributes) ioption(_interface_modifiers) ioption(PARTIAL)
  INTERFACE Ident ioption(type_parameter_list)
  ioption(interface_base) ioption(type_parameter_constraints_clauses)
  interface_body ioption(SEMI) {}

_interface_modifiers:
  _interface_access {}
| NEW _interface_access {}
| _interface_access NEW {}

%inline _interface_access:
  _class_access {}

interface_base:
  COLON interface_type_list {}

interface_body:
  LBRACE ioption(interface_member_declarations) RBRACE {}

interface_member_declarations:
  interface_member_declaration {}
| interface_member_declarations interface_member_declaration {}

interface_member_declaration:
  interface_method_declaration {}
| interface_property_declaration {}
| interface_event_declaration {}
| interface_indexer_declaration {}

interface_method_declaration:
  ioption(attributes) ioption(NEW) return_type Ident
  ioption(type_parameter_list)
  LPAR ioption(formal_parameter_list) RPAR
  ioption(type_parameter_constraints_clauses) SEMI {}

interface_property_declaration:
  ioption(attributes) ioption(NEW) _type Ident
  LBRACE interface_accessors RBRACE {}

interface_accessors:
  ioption(attributes) Ident SEMI { (* Ident="get" *) (* Ident="set" *) }
| ioption(attributes) Ident SEMI ioption(attributes) Ident SEMI {
  (* Ident1="set"; Ident2="get" *)
  (* Ident1="get"; Ident2="set" *) }

interface_event_declaration:
  ioption(attributes) ioption(NEW) EVENT _type Ident SEMI {}

interface_indexer_declaration:
  ioption(attributes) ioption(NEW) _type 
  LBRACKET formal_parameter_list RBRACKET
  LBRACE interface_accessors RBRACE {}

(* Enums (A.2.10, page 467, evince: p.490) *)

enum_declaration:
  ioption(attributes) ioption(_enum_modifiers) ENUM Ident
  ioption(enum_base) enum_body ioption(SEMI) {}

enum_base:
  COLON integral_type {}

enum_body:
  LBRACE ioption(enum_member_declarations) RBRACE {}
| LBRACE enum_member_declarations COMMA RBRACE {}

_enum_modifiers:
  _interface_access {}

enum_member_declarations:
  enum_member_declaration {}
| enum_member_declarations COMMA enum_member_declaration {}

enum_member_declaration:
  ioption(attributes) Ident {}
| ioption(attributes) Ident ASSIGN constant_expression {}

(* Delegates (A.2.11, page 468, evince: p.490) *)

delegate_declaration:
  ioption(attributes) ioption(_delegate_modifiers) DELEGATE return_type
  Ident ioption(type_parameter_list)
  LPAR ioption(formal_parameter_list) RPAR
  ioption(type_parameter_constraints_clauses) SEMI {}

_delegate_modifiers:
  _interface_modifiers {}

(* Attributes (A.2.12, page 468, evince: p.490) *)

(*
global_attributes:
  global_attribute_sections {}

global_attribute_sections:
  global_attribute_section {}
| global_attribute_sections global_attribute_section {}

global_attribute_section:
  LBRACKET global_attribute_target_specifier 
  attribute_list ioption(COMMA) RBRACKET {}

global_attribute_target_specifier:
  global_attribute_target COLON {}

global_attribute_target:
  Ident { (* Ident="assembly" *) }
*)

attributes:
  attribute_sections {}

attribute_sections:
  attribute_section {}
| attribute_sections attribute_section {}

attribute_section:
  LBRACKET ioption(attribute_target_specifier)
  attribute_list ioption(COMMA) RBRACKET {}

attribute_target_specifier:
  attribute_target COLON {}

attribute_target:
  Ident { (* Ident="field", "method", "param", "property",
                   "type" or "typevar". *) }
| EVENT {}
| RETURN {}

attribute_list:
  attribute {}
| attribute_list COMMA attribute {}

attribute:
  attribute_name ioption(attribute_arguments) {}

attribute_name:
  type_name {}

attribute_arguments:
  LPAR ioption(positional_argument_list) RPAR {}
| LPAR positional_argument_list COMMA named_argument_list RPAR {}
| LPAR named_argument_list RPAR {}

positional_argument_list:
  positional_argument {}
| positional_argument_list COMMA positional_argument {}

positional_argument:
  attribute_argument_expression {}

named_argument_list:
  named_argument {}
| named_argument_list COMMA named_argument {}

named_argument:
  Ident ASSIGN attribute_argument_expression {}

attribute_argument_expression:
  expression {}

(* Generics (A.2.13, page 470, evince: p.492) *)

type_parameter_list:
  LT type_parameters _gt {}

type_parameters:
  ioption(attributes) type_parameter {}
| type_parameters COMMA ioption(attributes) type_parameter {}

%inline type_parameter:
  Ident {}

type_argument_list:
  LT type_arguments _gt {}

type_arguments:
  type_argument {}
| type_arguments COMMA type_argument {}

type_argument:
  _type {}

type_parameter_constraints_clauses:
  type_parameter_constraints_clause {}
| type_parameter_constraints_clauses
  type_parameter_constraints_clause {}

type_parameter_constraints_clause:
  WHERE type_parameter COLON type_parameter_constraints {}

type_parameter_constraints:
  primary_constraint {}
| secondary_constraints {}
| constructor_constraint {}
| primary_constraint COMMA secondary_constraints {}
| primary_constraint COMMA constructor_constraint {}
| secondary_constraints COMMA constructor_constraint {}
| primary_constraint COMMA secondary_constraints
  COMMA constructor_constraint {}

primary_constraint:
  class_type {}
| CLASS {}
| STRUCT {}

secondary_constraints:
  interface_type {} (* May be [type_parameter] *)
(* | type_parameter {} *)
| secondary_constraints COMMA interface_type (* May be [type_parameter] *) {}
(* | secondary_constraints COMMA type_parameter {} *)

constructor_constraint:
  NEW LPAR RPAR {}

(* Unsafe code (A.3, page , evince: p.492) *)
