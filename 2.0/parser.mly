%{
(* Grammar for C# 2.0

   No preprocessor directives. No properties. No indexers.
   No unsafe code. No namespaces. No unsigned arithmetic.
   No global attributes, nor anonymous methods. No decimal type.
   No "break", no "continue". No enumerated. No extern.
   No "checked" and "unchecked" -> all expressions are unchecked (?)
   No "params", "volatile", "ref", "out", "typeof".
   No partial classes, no locks, no implicit nor explicit conversions.
   No delegates, no events, no iterators (foreach, yield), no goto.
   No operator declarations. No finalizers.
   No explicit interface implementation.
*)
%}

(* Keywords *)

%token ABSTRACT AS       BASE       BOOL      
%token BYTE     CASE     CATCH      CHAR      
%token CLASS    CONST                         DEFAULT_COLON DEFAULT_LPAR
%token          DO       DOUBLE     ELSE      
%token                              FALSE     FINALLY
%token          FLOAT    FOR        
%token IF                           INT       INTERFACE
%token INTERNAL IS                  LONG      
%token NEW      NULL     OBJECT     
%token OVERRIDE          PRIVATE    PROTECTED PUBLIC
%token READONLY          RETURN     SBYTE     SEALED
%token SHORT                        STATIC    STRING
%token STRUCT   SWITCH   THIS       THROW     TRUE
%token TRY               UINT       ULONG     
%token          USHORT              VIRTUAL   VOID
%token          WHILE    WHERE      

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

(* Basic concepts *)

compilation_unit:
  type_declarations {}

type_declarations:
  type_declaration {}
| type_declarations type_declaration {}

type_declaration:
  ioption(__modifiers) class_declaration {
  (* Leave out OVERRIDE, VIRTUAL and READONLY. *)
  }
| ioption(__modifiers) struct_declaration {
  (* Leave out ABSTRACT, OVERRIDE, READONLY, 
     SEALED, STATIC and VIRTUAL. *)
  }
| ioption(__modifiers) interface_declaration {
  (* Leave out ABSTRACT, OVERRIDE, READONLY, 
     SEALED, STATIC and VIRTUAL. *)
  }

type_name:
  Ident ioption(type_argument_list) {}
| type_name DOT Ident ioption(type_argument_list) {}

(* Types *)

_type:
  value_type { (* A [type_name] may be a [type_parameter]. *) }
| reference_type {}

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

(* Expressions *)

argument_list:
  argument {}
| argument_list COMMA argument {}

argument:
  expression { (* May be [variable_reference]. *) }

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
| default_value_expression {}

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

simple_name:
  Ident ioption(type_argument_list) {}

parenthesized_expression:
  LPAR expression RPAR {}

member_access:
  primary_expression DOT Ident ioption(type_argument_list) {}
| predefined_type DOT Ident ioption(type_argument_list) {}

predefined_type:
  BOOL {}
| BYTE {}
| CHAR {}
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

_gt:
  GT ioption(NOSPACE) {}

default_value_expression:
  DEFAULT_LPAR _type RPAR {}

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

(* Statements *)

statement:
  labeled_statement {}
| declaration_statement {}
| embedded_statement {}

embedded_statement:
  __open_embedded_statement {}
| __closed_embedded_statement {}

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
  switch_statement {}

__open_embedded_statement:
  __open_if_statement {}
| __open_while_statement {}
| __open_for_statement {}

__closed_embedded_statement:
  __closed_if_statement {}
| __closed_while_statement {}
| __closed_for_statement {}
| __other_embedded_statement {}

__other_embedded_statement:
  block {}
| empty_statement {}
| expression_statement {}
| selection_statement {}
| jump_statement {}
| try_statement {}

__open_if_statement:
  IF LPAR boolean_expression RPAR embedded_statement {}
| IF LPAR boolean_expression RPAR __closed_embedded_statement 
  ELSE __open_embedded_statement {}

__closed_if_statement:
  IF LPAR boolean_expression RPAR __closed_embedded_statement
  ELSE __closed_embedded_statement {}

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
| DEFAULT_COLON {}

while_statement:
  __open_while_statement {}
| __closed_while_statement {}

__open_while_statement:
  WHILE LPAR boolean_expression RPAR __open_embedded_statement {}

__closed_while_statement:
  WHILE LPAR boolean_expression RPAR __closed_embedded_statement {}

do_statement:
  DO embedded_statement WHILE LPAR boolean_expression RPAR SEMI {}

for_statement:
  __open_for_statement {}
| __closed_for_statement {}

__open_for_statement:
  FOR LPAR ioption(for_initializer) SEMI
           ioption(for_condition) SEMI
           ioption(for_iterator) RPAR __open_embedded_statement {}

__closed_for_statement:
  FOR LPAR ioption(for_initializer) SEMI
           ioption(for_condition) SEMI
           ioption(for_iterator) RPAR __closed_embedded_statement {}

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

jump_statement:
  return_statement {}
| throw_statement {}

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

(* Classes *)

class_declaration:
  CLASS Ident ioption(type_parameter_list) ioption(class_base)
  ioption(type_parameter_constraints_clauses)
  class_body ioption(SEMI) {}

class_base:
  COLON class_type_list {
   (* Either one [class_type], maybe followed by interfaces, or list of interfaces (leave out 
      any [OBJECT] and [STRING]) *)
  }

class_type_list:
  class_type {}
| class_type_list COMMA class_type {}

class_body:
  LBRACE ioption(class_member_declarations) RBRACE {}

class_member_declarations:
  class_member_declaration {}
| class_member_declarations class_member_declaration {}

class_member_declaration:
  constant_declaration {}
| field_declaration {}
| method_declaration {}
| constructor_declaration {}
| static_constructor_declaration {}
| type_declaration {}

constant_declaration:
  ioption(__modifiers) 
  CONST _type constant_declarators SEMI {
  (* Leave out ABSTRACT, OVERRIDE, SEALED, STATIC, VIRTUAL and 
     READONLY.*)
}

field_declaration:
  ioption(__modifiers)
  _type variable_declarators SEMI {
    (* Leave out ABSTRACT, OVERRIDE, SEALED and VIRTUAL. *)
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

method_declaration:
  method_header method_body {}

method_header:
  ioption(__modifiers) return_type
  member_name ioption(type_parameter_list)
  LPAR ioption(formal_parameter_list) RPAR
  ioption(type_parameter_constraints_clauses) {}

__modifiers:
  __modifier {}
| __modifiers __modifier {}

__modifier:
  ABSTRACT {}
| INTERNAL {}
| NEW {}
| OVERRIDE {}
| PRIVATE {}
| PROTECTED {}
| PUBLIC {}
| SEALED {}
| STATIC {}
| VIRTUAL {}
| READONLY {}

%inline return_type:
  _type {}
| VOID {}

member_name:
  Ident {}

method_body:
  block {}
| SEMI {}

formal_parameter_list:
  fixed_parameters {}

fixed_parameters:
  fixed_parameter {}
| fixed_parameters COMMA fixed_parameter {}

fixed_parameter:
  _type Ident {}

constructor_declaration:
  ioption(__modifiers)
  constructor_declarator constructor_body {
  (* Leave out ABSTRACT, NEW, OVERRIDE, SEALED, STATIC,
     VIRTUAL and READONLY. *)
}

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
  STATIC
  Ident LPAR RPAR static_constructor_body {}

static_constructor_body:
  block {}
| SEMI {}

(* Structures *)

struct_declaration:
  STRUCT Ident ioption(type_parameter_list)
  ioption(struct_interfaces) ioption(type_parameter_constraints_clauses)
  struct_body ioption(SEMI) {}

struct_interfaces:
  COLON class_type_list { (* Leave out any [STRING] or [OBJECT]. *) }

struct_body:
  LBRACE ioption(struct_member_declarations) RBRACE {}

struct_member_declarations:
  struct_member_declaration {}
| struct_member_declarations struct_member_declaration {}

struct_member_declaration:
  constant_declaration {}
| field_declaration {}
| method_declaration {}
| constructor_declaration {}
| static_constructor_declaration {}
| type_declaration {}

(* Arrays *)

array_type:
  non_array_type rank_specifiers {}

non_array_type:
  value_type {}
| class_type { (* [type_name] may be [class_type],
                  [interface_type], [delegate_type]
                  or [type_parameter]. *) }

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

(* Interfaces *)

interface_declaration:
  INTERFACE Ident ioption(type_parameter_list)
  ioption(interface_base) ioption(type_parameter_constraints_clauses)
  interface_body ioption(SEMI) {}

interface_base:
  COLON class_type_list { (* Leave out any [OBJECT] or [STRING]. *) }

interface_body:
  LBRACE ioption(interface_member_declarations) RBRACE {}

interface_member_declarations:
  interface_member_declaration {}
| interface_member_declarations interface_member_declaration {}

interface_member_declaration:
  interface_method_declaration {}

interface_method_declaration:
  ioption(NEW) return_type Ident
  ioption(type_parameter_list)
  LPAR ioption(formal_parameter_list) RPAR
  ioption(type_parameter_constraints_clauses) SEMI {}

(* Generics (A.2.13, page 470, evince: p.492) *)

type_parameter_list:
  LT type_parameters _gt {}

type_parameters:
  type_parameter {}
| type_parameters COMMA type_parameter {}

type_parameter:
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
  class_type {
    (* Leave out [OBJECT] and [STRING]. An [Ident] may be a [type_parameter]
       for an [interface_type]. *)
  }
| secondary_constraints COMMA class_type {
   (* Leave out [OBJECT] and [STRING]. An [Ident] may be a  [type_parameter]
      for an [interface_type]. *)
}

constructor_constraint:
  NEW LPAR RPAR {}

