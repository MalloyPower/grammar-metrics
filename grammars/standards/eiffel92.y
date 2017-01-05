/** Eiffel grammar
 ** To get this grammar, I started with the grammar available from
 **   http://www.gobosoft.com/eiffel/syntax/eiffel_y.html
 ** written by Eric Bezault, and reversed it back to the Eiffel standard
 **/

%token E_CHARACTER E_INTEGER E_REAL E_IDENTIFIER E_STRING E_BIT E_BITTYPE
%token E_BANGBANG E_ARROW E_DOTDOT E_LARRAY E_RARRAY E_ASSIGN E_REVERSE
%token E_ALIAS E_ALL E_AS E_CHECK E_CLASS E_CREATION E_DEBUG E_DEFERRED
%token E_DO E_ELSE E_ELSEIF E_END E_ENSURE E_EXPANDED E_EXPORT
%token E_EXTERNAL E_FALSE E_FEATURE E_FROM E_FROZEN E_IF E_INDEXING
%token E_INFIX E_INHERIT E_INSPECT E_INVARIANT E_IS E_LIKE E_LOCAL
%token E_LOOP E_OBSOLETE E_ONCE E_PREFIX E_REDEFINE E_RENAME E_REQUIRE
%token E_RESCUE E_RETRY E_SELECT E_SEPARATE E_STRIP E_THEN E_TRUE
%token E_UNDEFINE E_UNIQUE E_UNTIL E_VARIANT E_WHEN E_CURRENT E_RESULT
%token E_PRECURSOR

%left E_IMPLIES
%left E_OR E_XOR
%left E_AND
%left "=" E_NE "<" ">" E_LE E_GE
%left "+" "-"
%left "*" "/" E_DIV E_MOD
%right "^"
%left E_FREEOP
%right E_NOT E_OLD

/* Pass through comments (not really, of course) */
%token E_COMMENT  

/* I think the following is a token (pg 68 */
%token E_FREE_OPERATOR


%start Class_declaration

%%
/* ---------------------------------------------------------------------- */

Class_declaration
  : [  Indexing ] Class_header [  Formal_generics ] [  Obsolete ]
    [  Inheritance ] [  Creators ] [  Features ] [  Invariant ] 
    E_END [ "--" E_CLASS Class_name ]
  ;

/* ---------------------------------------------------------------------- */

Indexing
  :  E_INDEXING Index_list
  ;

Index_list
  : [ Index_clause { ";" Index_clause  } ]
  ;

Index_clause
  : [ Index ] Index_terms
  ;
Index 
  : E_IDENTIFIER ":"
  ;
Index_terms
  : Index_value {  "," Index_value }
  ;

Index_value
  : E_IDENTIFIER
  | Manifest_constant
  ;

/* ---------------------------------------------------------------------- */

Class_header
  : [  Header_mark ] E_CLASS Class_name
  ;

Header_mark
  : E_DEFERRED
  | E_EXPANDED
  ;

Class_name
  : E_IDENTIFIER
  ;


/* ---------------------------------------------------------------------- */

Formal_generics
  : "[" Formal_generic_list "]"
  ;

Formal_generic_list
  :  [ Formal_generic { "," Formal_generic } ]
  ;

Formal_generic
  : Formal_generic_name  [  Constraint ]
  ;
Formal_generic_name
  : E_IDENTIFIER
  ;
Constraint 
  : E_ARROW Class_type
  ;

/* ---------------------------------------------------------------------- */

Obsolete
  :  E_OBSOLETE Message
  ;
Message
  : E_STRING
  ;

/* ---------------------------------------------------------------------- */

Features
  : E_FEATURE Feature_clause { E_FEATURE Feature_clause }
  ;

Feature_clause
  : [  Clients ]  [ Header_comment ] Feature_declaration_list
  ;
Feature_declaration_list
  : [ Feature_declaration { ";" Feature_declaration } ]
  ;

Header_comment
  : Comment
  ;

/* ---------------------------------------------------------------------- */

Feature_declaration
  : New_feature_list Declaration_body
  ;

Declaration_body
  : [  Formal_arguments ] [  Type_mark ] [  Constant_or_routine ]
  ;

Constant_or_routine
  : E_IS Feature_value
  ;

Feature_value
  : Manifest_constant
  | E_UNIQUE
  | Routine
  ;

/* ---------------------------------------------------------------------- */

New_feature_list
  : New_feature { "," New_feature }
  ;

New_feature
  : [ E_FROZEN ] Feature_name
  ;

/* ---------------------------------------------------------------------- */

Feature_name
  : E_IDENTIFIER
  | Prefix
  | Infix
  ;
Prefix
  : E_PREFIX '"' Prefix_operator '"'
  ;
Infix
  : E_INFIX '"' Infix_operator '"'
  ;
Prefix_operator
  : Unary
  | Free_operator
  ;
Infix_operator
  : Binary
  | Free_operator
  ;
Free_operator
  : E_FREE_OPERATOR
  ;
/* ---------------------------------------------------------------------- */

Unary
  : E_NOT | "+" | "-"
  ;
Binary
  : "+" | "-" | "*" | "/"
  | "<" | ">" | "<=" | ">="
  | "//" | "\\\\" | "^"
  | E_AND | E_OR | E_XOR
  | E_AND E_THEN | E_OR E_ELSE | E_IMPLIES
  ;

/* ---------------------------------------------------------------------- */

Inheritance
  : E_INHERIT Parent_list
  ;

Parent_list
  : [ Parent { ";" Parent } ]
  ;

Parent
  : Class_type [  Feature_adaptation ]
  ;

Feature_adaptation
  : [ Rename ] [ New_exports ] [Undefine ] [ Redefine ] [Select ] E_END
  ;


/* ---------------------------------------------------------------------- */

Rename
  : E_RENAME Rename_list
  ;

Rename_list
  : [ Rename_pair { "," Rename_pair } ]
  ;

Rename_pair
  : Feature_name E_AS Feature_name
  ;

/* ---------------------------------------------------------------------- */

Clients
  : "{" Class_list "}"
  ;
Class_list
  : [ Class_name { "," Class_name } ]
  ;

/* ---------------------------------------------------------------------- */

New_exports
  : E_EXPORT New_export_list
  ;

New_export_list
  : [ New_export_item { ";" New_export_item } ]
  ;

New_export_item
  : Clients Feature_set
  ;

Feature_set
  : Feature_list
  | E_ALL
  ;

Feature_list
  : [ Feature_name { "," Feature_name } ]
  ;


/* ---------------------------------------------------------------------- */

Formal_arguments
  : "(" Entity_declaration_list ")"
  ;

Entity_declaration_list
  : [ Entity_declaration_group {";" Entity_declaration_group } ]
  ;

Entity_declaration_group
  : Identifier_list  Type_mark
  ;

Identifier_list
  : E_IDENTIFIER { "," E_IDENTIFIER }
  ;

Type_mark
  : ":" Type
  ;

/* ---------------------------------------------------------------------- */

Routine
  : [ Obsolete ] [ Header_comment ] [ Precondition ] [  Local_declarations ]
    Routine_body [ Postcondition ] [ Rescue ] E_END
    [ "--" Feature_name ]
  ;


/* ---------------------------------------------------------------------- */

Routine_body
  : Effective
  | Deferred
  ;
Effective
  : Internal
  | External
  ;
Internal
  : Routine_mark Compound
  ;
Routine_mark
  : E_DO
  | E_ONCE
  ;
Deferred
  : E_DEFERRED
  ;

/* ---------------------------------------------------------------------- */

Local_declarations
  :  E_LOCAL Entity_declaration_list
  ;

/* ---------------------------------------------------------------------- */

Instruction
  : Creation
  | Call
  | Assignment
  | Assignment_attempt
  | Conditional
  | Multi_branch
  | Loop
  | Debug
  | Check
  | Retry
  ;

/* ---------------------------------------------------------------------- */

Precondition
  : E_REQUIRE [ E_ELSE ] Assertion
  ;

Postcondition
  : E_ENSURE [ E_THEN ] Assertion
  ;

Invariant
  : E_INVARIANT Assertion
  ;

Assertion
  : [ Assertion_clause { ";" Assertion_clause } ]
  ;

Assertion_clause
  : [ Tag_mark ] Unlabeled_assertion_clause
  ;
Unlabeled_assertion_clause
  : Boolean_expression
  | Comment
  ;
Tag_mark
  : Tag ":"
  ;
Tag 
  : E_IDENTIFIER
  ;

/* ---------------------------------------------------------------------- */

Old 
  : E_OLD Expression
  ;
Check
  : E_CHECK Assertion E_END
  ;
Variant
  : E_VARIANT [ Tag_mark ] Expression
  ;
Redefine
  : E_REDEFINE Feature_list
  ;
Undefine
  : E_UNDEFINE Feature_list
  ;
Select
  : E_SELECT  Feature_list
  ;

/* ---------------------------------------------------------------------- */

Type
  : Class_type
  | Class_type_expanded
  | Formal_generic_name
  | Anchored
  | Bit_type
  ;
Class_type
  : Class_name [ Actual_generics ]
  ;
Actual_generics
  : "[" Type_list "]"
  ;
Type_list
  : [ Type { "," Type } ]
  ;
Class_type_expanded
  : E_EXPANDED Class_type
  ;
Bit_type
  : E_BITTYPE Constant
  ;
Anchored
  : E_LIKE Anchor
  ;
Anchor
  : E_IDENTIFIER 
  | E_CURRENT
  ;
/* ---------------------------------------------------------------------- */

Compound
  : [ Instruction { ";" Instruction } ]
  ;

/* ---------------------------------------------------------------------- */

Conditional
  : E_IF Then_part_list [ Else_part ] E_END
  ;
Then_part_list 
  : Then_part { E_ELSEIF Then_part }
  ;
Then_part
  : Boolean_expression E_THEN Compound
  ;
Else_part
  : E_ELSE Compound
  ;

/* ---------------------------------------------------------------------- */

Multi_branch
  : E_INSPECT Expression [When_part_list] [Else_part] E_END
  ;

When_part_list
  : E_WHEN When_part { E_WHEN When_part }
  ;
When_part
  : Choices E_THEN Compound
  ;

Choices
  : [ Choice { "," Choice } ]
  ;

Choice
  : Constant
  | Interval
  ;
Interval
  : Integer_interval
  | Character_interval
  ;
Integer_interval
  : Integer_constant E_DOTDOT Integer_constant
  ;
Character_interval
  : Character_constant E_DOTDOT Character_constant
  ;

/* ---------------------------------------------------------------------- */

Loop
  : Initialization [ Invariant ] [ Variant ] Loop_body E_END
  ;
Initialization
  :  E_FROM Compound
  ;
Loop_body
  :  Exit E_LOOP Compound 
  ;
Exit
  : E_UNTIL Boolean_expression
  ;

/* ---------------------------------------------------------------------- */
Debug
  : E_DEBUG [ Debug_keys ] Compound E_END
  ;

Debug_keys
  : "(" Debug_key_list ")"
  ;

Debug_key_list
  : [ Debug_key { "," Debug_key } ]
  ;
Debug_key
  : Manifest_string
  ;
/* ---------------------------------------------------------------------- */
Rescue
  : E_RESCUE Compound
  ;
Retry
  : E_RETRY
  ;
/* ---------------------------------------------------------------------- */

Unique
  : E_UNIQUE
  ;

/* ---------------------------------------------------------------------- */

Entity
  : Writable
  | Read_only
  ;
Writable
  : Attribute
  | Local
  ;
Attribute
  : E_IDENTIFIER
  ;
Local
  : E_IDENTIFIER
  | E_RESULT
  ;
Read_only
  : Formal
  | E_CURRENT
  ;
Formal
  : E_IDENTIFIER
  ;

/* ---------------------------------------------------------------------- */

Creators
  : E_CREATION Creation_clause { E_CREATION Creation_clause }
  ;

Creation_clause
  : [ Clients ] [ Header_comment ] Feature_list
  ;

/* ---------------------------------------------------------------------- */

Creation
  : "!" [ Type ] "!" Writable [ Creation_call ]
  ;

Creation_call
  :  "." Unqualified_call
  ;

/* ---------------------------------------------------------------------- */

Assignment
  : Writable E_ASSIGN Expression
  ;
Assignment_attempt
  : Writable E_REVERSE Expression
  ;


/* ---------------------------------------------------------------------- */

Call
  : [Parenthesized_qualifier] Call_chain
  ;
Parenthesized_qualifier
  : Parenthesized "."
  ;
Call_chain
  : Unqualified_call { "." Unqualified_call }
  ;
Unqualified_call
  : Entity [ Actuals ]
  ;
/* ---------------------------------------------------------------------- */

Actuals
  : "(" Actual_list ")"
  ;

Actual_list
  : [ Actual { "," Actual } ]
  ;

Actual
  : Expression
  | Address
  ;
Address
  : "$" E_IDENTIFIER
  ;

/* ---------------------------------------------------------------------- */

Expression
  : Call
  | Operator_expression
  | Equality
  | Manifest_constant
  | Manifest_array
  | Old
  | Strip
  ;
Boolean_expression
  : Expression
  ;

/* ---------------------------------------------------------------------- */
Equality
  : Expression Comparison Expression
  ;
Comparison
  : "=" 
  | "/="
  ;

/* ---------------------------------------------------------------------- */

Operator_expression
  : Parenthesized
  | Unary_expression
  | Binary_expression
  ;
Parenthesized
  : "(" Expression ")"
  ;
Unary_expression
  : Prefix_operator Expression
  ;
Binary_expression
  : Expression Infix_operator Expression
  ;

/* ---------------------------------------------------------------------- */

Constant
  : Manifest_constant
  | Constant_attribute
  ;
Constant_attribute
  : Entity
  ;

/* ---------------------------------------------------------------------- */

Manifest_constant
  : Boolean_constant
  | Character_constant
  | Integer_constant
  | Real_constant
  | Manifest_string
  | Bit_constant
  ;
Sign
  : "+"
  | "-"
  ;
Integer_constant
  : [Sign] E_INTEGER
  ;
Character_constant
  : E_CHARACTER
  ;
Boolean_constant
  : E_TRUE
  | E_FALSE
  ;
Real_constant
  : [Sign] E_REAL
  ;
Manifest_string
  : E_STRING
  ;
Bit_constant
  : E_BIT
  ;
/* ---------------------------------------------------------------------- */
Manifest_array
  : "<<" Expression_list ">>"
  ;
Expression_list
  : [ Expression { "," Expression } ]
  ;

/* ---------------------------------------------------------------------- */
Strip
  : E_STRIP "(" Attribute_list ")"
  ;
Attribute_list
  : [ E_IDENTIFIER { "," E_IDENTIFIER } ]
  ;

/* ---------------------------------------------------------------------- */
External
  : E_EXTERNAL Language_name [ External_name ]
  ;
Language_name
  : Manifest_string
  ;
External_name
  : E_ALIAS Manifest_string
  ;
/* ---------------------------------------------------------------------- */
Comment
  : E_COMMENT
  ;
/* ---------------------------------------------------------------------- */

%%


