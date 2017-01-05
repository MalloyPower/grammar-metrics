/* C# grammar, 
 * based on version 0.28 of the C# Language Specification (of 5/7/2001)
 * and thus probably in dire need of being updated.
 * Current ECMA-standard C# is at:
 *   http://www.ecma-international.org/publications/standards/Ecma-334.htm
 */


/* C.1.4 Tokens */
%token IDENTIFIER 
%token INTEGER_LITERAL REAL_LITERAL CHARACTER_LITERAL STRING_LITERAL


/* C.1.7 KEYWORDS */ 
%token  ABSTRACT AS BASE BOOL BREAK
%token  BYTE CASE CATCH CHAR CHECKED
%token  CLASS CONST CONTINUE DECIMAL DEFAULT
%token  DELEGATE DO DOUBLE ELSE ENUM
%token  EVENT EXPLICIT EXTERN FALSE FINALLY
%token  FIXED FLOAT FOR FOREACH GOTO
%token  IF IMPLICIT IN INT INTERFACE
%token  INTERNAL IS LOCK LONG NAMESPACE
%token  NEW NULL_LITERAL OBJECT OPERATOR OUT
%token  OVERRIDE PARAMS PRIVATE PROTECTED PUBLIC
%token  READONLY REF RETURN SBYTE SEALED
%token  SHORT SIZEOF STACKALLOC STATIC STRING
%token  STRUCT SWITCH THIS THROW TRUE
%token  TRY TYPEOF UINT ULONG UNCHECKED
%token  UNSAFE USHORT USING VIRTUAL VOID
%token  VOLATILE WHILE

/* THE ONES THEY SEEM TO HAVE MISSED */
%token ASSEMBLY FIELD METHOD MODULE PARAM PROPERTY 
%token GET SET 
%token ADD REMOVE
%token TYPE

/*** MULTI-CHARACTER OPERATORS ***/
%token PLUSEQ MINUSEQ STAREQ DIVEQ MODEQ
%token XOREQ  ANDEQ   OREQ LTLT GTGT GTGTEQ LTLTEQ EQEQ NOTEQ
%token LEQ GEQ ANDAND OROR PLUSPLUS MINUSMINUS ARROW

%start compilation_unit  /* I think */

%%

/***** C.1.8 Literals *****/
literal
  : boolean_literal
  | INTEGER_LITERAL
  | REAL_LITERAL
  | CHARACTER_LITERAL
  | STRING_LITERAL
  | NULL_LITERAL
  ;
boolean_literal
  : TRUE
  | FALSE
  ;
/********** C.2 Syntactic grammar **********/

/***** C.2.1 Basic concepts *****/
namespace_name
  : namespace_or_type_name
  ;

type_name
  : namespace_or_type_name
  ;
namespace_or_type_name
  : IDENTIFIER
  | namespace_or_type_name   '.'   IDENTIFIER
  ;
/***** C.2.2 Types *****/
type
  : value_type
  | reference_type
  | pointer_type
  ;
value_type
  : struct_type
  | enum_type
  ;
struct_type
  : type_name
  | simple_type
  ;
simple_type
  : numeric_type
  | BOOL
  ;
numeric_type
  : integral_type
  | floating_point_type
  | DECIMAL
  ;
integral_type
  : SBYTE | BYTE | SHORT | USHORT | INT | UINT | LONG | ULONG | CHAR
  ;
floating_point_type
  : FLOAT 
  | DOUBLE
  ;
enum_type
  : type_name
  ;
reference_type
  : class_type
  | interface_type
  | array_type
  | delegate_type
  ;
class_type
  : type_name
  | OBJECT
  | STRING
  ;
interface_type
  : type_name
  ;
pointer_type
  : unmanaged_type   '*'
  | VOID   '*'
  ;
unmanaged_type
  : type
  ;
array_type
  : non_array_type   rank_specifiers
  ;
non_array_type:
  | type
  ;
rank_specifiers
  : rank_specifier
  | rank_specifiers   rank_specifier
  ;
rank_specifier
  : '['   [ dim_separators ]   ']'
  ;
dim_separators
  : ','
  | dim_separators   ','
  ;
delegate_type
  : type_name
  ;
/***** C.2.3 Variables *****/
variable_reference
  : expression
  ;
/***** C.2.4 Expressions *****/
argument_list
  : argument
  | argument_list   ','   argument
  ;
argument
  : expression
  | REF   variable_reference
  | OUT   variable_reference
  ;
primary_expression
  : array_creation_expression
  | primary_expression_no_array_creation
  ;
primary_expression_no_array_creation
  : literal
  | simple_name
  | parenthesized_expression
  | member_access
  | invocation_expression
  | element_access
  | this_access
  | base_access
  | post_increment_expression
  | post_decrement_expression
  | new_expression
  | typeof_expression
  | sizeof_expression
  | checked_expression
  | unchecked_expression
  | pointer_member_access
  | pointer_element_access
  ;
simple_name
  : IDENTIFIER
  ;
parenthesized_expression
  : '('   expression   ')'
  ;
member_access
  : primary_expression   '.'   IDENTIFIER
  | predefined_type   '.'  IDENTIFIER
  ;
predefined_type 
  : BOOL | BYTE | CHAR | DECIMAL | DOUBLE | FLOAT | INT | LONG
  | OBJECT | SBYTE | SHORT | STRING | UINT | ULONG | USHORT
  ;
invocation_expression
  : primary_expression   '('   [ argument_list ]   ')'
  ;
element_access
  : primary_expression_no_array_creation   '['   expression_list   ']'
  ;
pointer_element_access
  : primary_expression_no_array_creation   '['   expression   ']'
  ;
expression_list
  : expression
  | expression_list   ','   expression
  ;
this_access
  : THIS
  ;
base_access
  : BASE   '.'   IDENTIFIER
  | BASE   '['   expression_list   ']'
  ;
post_increment_expression
  : primary_expression   PLUSPLUS
  ;
post_decrement_expression
  : primary_expression   MINUSMINUS
  ;
new_expression
  : object_creation_expression
  | array_creation_expression
  | delegate_creation_expression
  ;
object_creation_expression
  : NEW   type   '('   [ argument_list ]   ')'
  ;
array_creation_expression
  : NEW non_array_type  '[' expression_list ']' [ rank_specifiers ] [ array_initializer ]
  | NEW   array_type   array_initializer
  ;
delegate_creation_expression
  : NEW   delegate_type   '('   expression   ')'
  ;
typeof_expression
  : TYPEOF   '('   type   ')'
  | TYPEOF   '('   VOID   ')'
  ;
checked_expression
  : CHECKED   '('   expression   ')'
  ;
unchecked_expression
  : UNCHECKED   '('   expression   ')'
  ;
pointer_member_access
  : primary_expression   ARROW   IDENTIFIER
  ;
pointer_indirection_expression
  : '*'   unary_expression
  ;
addressof_expression
  : '&'   unary_expression
  ;
sizeof_expression
  : SIZEOF   '('   unmanaged_type   ')'
  ;
unary_expression
  : primary_expression
  | '+'   unary_expression
  | '-'   unary_expression
  | '!'   unary_expression
  | '~'   unary_expression
  | '*'   unary_expression
  | pre_increment_expression
  | pre_decrement_expression
  | cast_expression
  | pointer_indirection_expression
  | addressof_expression
  ;
pre_increment_expression
  : PLUSPLUS   unary_expression
  ;
pre_decrement_expression
  : MINUSMINUS   unary_expression
  ;
cast_expression
  : '('   type   ')'   unary_expression
  ;
multiplicative_expression
  : unary_expression
  | multiplicative_expression   '*'   unary_expression
  | multiplicative_expression   '/'   unary_expression
  | multiplicative_expression   '%'   unary_expression
  ;
additive_expression
  : multiplicative_expression
  | additive_expression   '+'   multiplicative_expression
  | additive_expression   '-'   multiplicative_expression
  ;
shift_expression
  : additive_expression 
  | shift_expression   LTLT   additive_expression
  | shift_expression   GTGT   additive_expression
  ;
relational_expression
  : shift_expression
  | relational_expression   '<'   shift_expression
  | relational_expression   '>'   shift_expression
  | relational_expression   LEQ   shift_expression
  | relational_expression   GEQ   shift_expression
  | relational_expression   IS   type
  | relational_expression   AS   type
  ;
equality_expression
  : relational_expression
  | equality_expression   EQEQ   relational_expression
  | equality_expression   NOTEQ   relational_expression
  ;
and_expression
  : equality_expression
  | and_expression   '&'   equality_expression
  ;
exclusive_or_expression
  : and_expression
  | exclusive_or_expression   '^'   and_expression
  ;
inclusive_or_expression
  : exclusive_or_expression
  | inclusive_or_expression   '|'   exclusive_or_expression
  ;
conditional_and_expression
  : inclusive_or_expression
  | conditional_and_expression   ANDAND   inclusive_or_expression
  ;
conditional_or_expression
  : conditional_and_expression
  | conditional_or_expression   OROR   conditional_and_expression
  ;
conditional_expression
  : conditional_or_expression
  | conditional_or_expression   '?'   expression   ':'   expression
  ;
assignment
  : unary_expression   assignment_operator   expression
  ;
assignment_operator
  : '=' | PLUSEQ | MINUSEQ | STAREQ | DIVEQ | MODEQ 
  | XOREQ | ANDEQ | OREQ | GTGTEQ | LTLTEQ 
  ;
expression
  : conditional_expression
  | assignment
  ;
constant_expression
  : expression
  ;
boolean_expression
  : expression
  ;
/***** C.2.5 Statements *****/
statement
  : labeled_statement
  | declaration_statement
  | embedded_statement
  ;
embedded_statement
  : block
  | empty_statement
  | expression_statement
  | selection_statement
  | iteration_statement
  | jump_statement
  | try_statement
  | checked_statement
  | unchecked_statement
  | lock_statement
  | using_statement
  | unsafe_statement
  | fixed_statement
  ;
block
  : '{'   [ statement_list ]   '}'
  ;
statement_list
  : statement
  | statement_list   statement
  ;
empty_statement
  : ';'
  ;
labeled_statement
  : IDENTIFIER   ':'   statement
  ;
declaration_statement
  : local_variable_declaration   ';'
  | local_constant_declaration   ';'
  ;
local_variable_declaration
  : type   variable_declarators
  ;
variable_declarators
  : variable_declarator
  | variable_declarators   ','   variable_declarator
  ;
variable_declarator
  : IDENTIFIER
  | IDENTIFIER   '='   variable_initializer
  ;
variable_initializer
  : expression
  | array_initializer
  | stackalloc_initializer
  ;
stackalloc_initializer
  : STACKALLOC   unmanaged_type   '['   expression   ']'
  ; 
local_constant_declaration
  : CONST   type   constant_declarators
  ;
constant_declarators
  : constant_declarator
  | constant_declarators   ','   constant_declarator
  ;
constant_declarator
  : IDENTIFIER   '='   constant_expression
  ;
expression_statement
  : statement_expression   ';'
  ;
statement_expression
  : invocation_expression
  | object_creation_expression
  | assignment
  | post_increment_expression
  | post_decrement_expression
  | pre_increment_expression
  | pre_decrement_expression
  ;
selection_statement
  : if_statement
  | switch_statement
  ;
if_statement
  : IF   '('   boolean_expression   ')'   embedded_statement
  | IF   '('   boolean_expression   ')'   embedded_statement   ELSE   embedded_statement
  ;
switch_statement
  : SWITCH   '('   expression   ')'   switch_block
  ;
switch_block
  : '{'   [ switch_sections ]   '}'
  ;
switch_sections
  : switch_section
  | switch_sections   switch_section
  ;
switch_section
  : switch_labels   statement_list
  ;
switch_labels
  : switch_label
  | switch_labels   switch_label
  ;
switch_label
  : CASE   constant_expression   ':'
  | DEFAULT   ':'
  ;
iteration_statement
  : while_statement
  | do_statement
  | for_statement
  | foreach_statement
  ;
unsafe_statement
  : UNSAFE   block
  ;
while_statement
  : WHILE   '('   boolean_expression   ')'   embedded_statement
  ;
do_statement
  : DO   embedded_statement   WHILE   '('   boolean_expression   ')'   ';'
  ;
for_statement
  : FOR   '('   [ for_initializer ]   ';'   [ for_condition ]   ';'   [ for_iterator ]   ')'   embedded_statement
  ;
for_initializer
  : local_variable_declaration
  | statement_expression_list
  ;
for_condition
  : boolean_expression
  ;
for_iterator
  : statement_expression_list
  ;
statement_expression_list
  : statement_expression
  | statement_expression_list   ','   statement_expression
  ;
foreach_statement
  : FOREACH   '('   type   IDENTIFIER   IN   expression   ')'   embedded_statement
  ;
jump_statement
  : break_statement
  | continue_statement
  | goto_statement
  | return_statement
  | throw_statement
  ;
break_statement
  : BREAK   ';'
  ;
continue_statement
  : CONTINUE   ';'
  ;
goto_statement
  : GOTO   IDENTIFIER   ';'
  | GOTO   CASE   constant_expression   ';'
  | GOTO   DEFAULT   ';'
  ;
return_statement
  : RETURN   [ expression ]   ';'
  ;
throw_statement
  : THROW   [ expression ]   ';'
  ;
try_statement
  : TRY   block   catch_clauses
  | TRY   block   finally_clause
  | TRY   block   catch_clauses   finally_clause
  ;
catch_clauses
  : specific_catch_clauses   [ general_catch_clause ]
  | [ specific_catch_clauses ]   general_catch_clause
  ;
specific_catch_clauses
  : specific_catch_clause
  | specific_catch_clauses   specific_catch_clause
  ;
specific_catch_clause
  : CATCH   '('   class_type   [ IDENTIFIER ]   ')'   block
  ;
general_catch_clause
  : CATCH   block
  ;
finally_clause
  : FINALLY   block
  ;
checked_statement
  : CHECKED   block
  ;
unchecked_statement
  : UNCHECKED   block
  ;
lock_statement
  : LOCK   '('   expression   ')'   embedded_statement
  ;
using_statement
  : USING   '('    resource_acquisition   ')'    embedded_statement
  ;
resource_acquisition
  : local_variable_declaration
  | expression
  ;
fixed_statement
  : FIXED   '('   pointer_type   fixed_pointer_declarators   ')'   embedded_statement
  ;
fixed_pointer_declarators
  : fixed_pointer_declarator
  | fixed_pointer_declarators   ','   fixed_pointer_declarator
  ;
fixed_pointer_declarator
  : IDENTIFIER   '='   fixed_pointer_initializer
  ;
fixed_pointer_initializer
  : '&'   variable_reference
  | expression 
  ;
compilation_unit
  : [ using_directives ]  [ attributes ]    [ namespace_member_declarations ]
  ;
namespace_declaration
  : NAMESPACE   qualified_identifier   namespace_body   [ ',' ]
  ;
qualified_identifier
  : IDENTIFIER
  | qualified_identifier   '.'   IDENTIFIER
  ;
namespace_body
  : '{'   [ using_directives ]   [ namespace_member_declarations ]   '}'
  ;
using_directives
  : using_directive
  | using_directives   using_directive
  ;
using_directive
  : using_alias_directive
  | using_namespace_directive
  ;
using_alias_directive
  : USING   IDENTIFIER   '='   namespace_or_type_name   ';'
  ;
using_namespace_directive
  : USING   namespace_name   ';'
  ;
namespace_member_declarations
  : namespace_member_declaration
  | namespace_member_declarations   namespace_member_declaration
  ;
namespace_member_declaration
  : namespace_declaration
  | type_declaration
  ;
type_declaration
  : class_declaration
  | struct_declaration
  | interface_declaration
  | enum_declaration
  | delegate_declaration
  ;

/***** C.2.6 Classes *****/
class_declaration
  : [ attributes ]   [ class_modifiers ]   CLASS   IDENTIFIER   [ class_base ]   class_body   [ ',' ]
  ;
class_modifiers
  : class_modifier
  | class_modifiers   class_modifier
  ;
class_modifier
  : NEW
  | PUBLIC
  | PROTECTED
  | INTERNAL
  | PRIVATE
  | ABSTRACT
  | SEALED
  | UNSAFE
  ;
class_base
  : ':'   class_type
  | ':'   interface_type_list
  | ':'   class_type   ','   interface_type_list
  ;
interface_type_list
  : interface_type
  | interface_type_list   ','   interface_type
  ;
class_body
  : '{'   [ class_member_declarations ]   '}'
  ;
class_member_declarations
  : class_member_declaration
  | class_member_declarations   class_member_declaration
  ;
class_member_declaration
  : constant_declaration
  | field_declaration
  | method_declaration
  | property_declaration
  | event_declaration
  | indexer_declaration
  | operator_declaration
  | constructor_declaration
  | destructor_declaration
  | static_constructor_declaration
  | type_declaration
  ;
constant_declaration
  : [ attributes ]   [ constant_modifiers ]   CONST   type   constant_declarators   ';'
  ;
constant_modifiers
  : constant_modifier
  | constant_modifiers   constant_modifier
  ;
constant_modifier
  : NEW
  | PUBLIC
  | PROTECTED
  | INTERNAL
  | PRIVATE
  ;
field_declaration
  : [ attributes ]   [ field_modifiers ]   type   variable_declarators   ';'
  ;
field_modifiers
  : field_modifier
  | field_modifiers   field_modifier
  ;
field_modifier
  : NEW
  | PUBLIC
  | PROTECTED
  | INTERNAL
  | PRIVATE
  | STATIC
  | READONLY
  | UNSAFE
  ;
method_declaration
  : method_header   method_body
  ;
method_header
  : [ attributes ]   [ method_modifiers ]   return_type   member_name   '('   [ formal_parameter_list ]   ')'
  ;
method_modifiers
  : method_modifier
  | method_modifiers   method_modifier
  ;
method_modifier
  : NEW
  | PUBLIC
  | PROTECTED
  | INTERNAL
  | PRIVATE
  | STATIC
  | VIRTUAL
  | SEALED
  | OVERRIDE
  | ABSTRACT
  | EXTERN
  | UNSAFE
  ;
return_type
  : type
  | VOID
  ;
member_name
  : IDENTIFIER
  | interface_type   '.'   IDENTIFIER
  ;
method_body
  : block
  | ';'
  ;
formal_parameter_list
  : fixed_parameters
  | fixed_parameters   ','   parameter_array
  | parameter_array
  ;
fixed_parameters
  : fixed_parameter
  | fixed_parameters   ','   fixed_parameter
  ;
fixed_parameter
  : [ attributes ]   [ parameter_modifier ]   type   IDENTIFIER
  ;
parameter_modifier
  : REF
  | OUT
  ;
parameter_array
  : [ attributes ]   PARAMS   array_type   IDENTIFIER
  ;
property_declaration
  : [ attributes ]   [ property_modifiers ]   type   member_name   '{'   accessor_declarations   '}'
  ;
property_modifiers
  : property_modifier
  | property_modifiers   property_modifier
  ;
property_modifier
  : NEW
  | PUBLIC
  | PROTECTED
  | INTERNAL
  | PRIVATE
  | STATIC
  | VIRTUAL
  | SEALED
  | OVERRIDE
  | ABSTRACT
  | EXTERN
  | UNSAFE
  ;
accessor_declarations
  : get_accessor_declaration   [ set_accessor_declaration ]
  | set_accessor_declaration   [ get_accessor_declaration ]
  ;
get_accessor_declaration
  : [ attributes ]   GET   accessor_body
  ;
set_accessor_declaration
  : [ attributes ]   SET   accessor_body
  ;
accessor_body
  : block
  | ';'
  ;
event_declaration
  : [ attributes ]   [ event_modifiers ]   EVENT   type   variable_declarators   ';'
  | [ attributes ]   [ event_modifiers ]   EVENT   type   member_name   '{'   event_accessor_declarations   '}'
  ;
event_modifiers
  : event_modifier
  | event_modifiers   event_modifier
  ;
event_modifier
  : NEW
  | PUBLIC
  | PROTECTED
  | INTERNAL
  | PRIVATE
  | STATIC
  | VIRTUAL
  | SEALED
  | OVERRIDE
  | ABSTRACT
  | EXTERN
  | UNSAFE
  ;
event_accessor_declarations
  : add_accessor_declaration   remove_accessor_declaration
  | remove_accessor_declaration   add_accessor_declaration
  ;
add_accessor_declaration
  : [ attributes ]   ADD   block
  ;
remove_accessor_declaration
  : [ attributes ]   REMOVE   block
  ;
indexer_declaration
  : [ attributes ]   [ indexer_modifiers ]   indexer_declarator   '{'   accessor_declarations   '}'
  ;
indexer_modifiers
  : indexer_modifier
  | indexer_modifiers   indexer_modifier
  ;
indexer_modifier
  : NEW
  | PUBLIC
  | PROTECTED
  | INTERNAL
  | PRIVATE 
  | VIRTUAL
  | SEALED
  | OVERRIDE
  | ABSTRACT
  | UNSAFE
  ;
indexer_declarator
  : type   THIS   '['   formal_parameter_list   ']'
  | type   interface_type   '.'   THIS   '['   formal_parameter_list   ']'
  ;
operator_declaration
  : [ attributes ]   operator_modifiers   operator_declarator   operator_body
  ;
operator_modifiers
  : operator_modifier
  | operator_modifiers   operator_modifier
  ;
operator_modifier
  : PUBLIC
  | STATIC
  | EXTERN
  | UNSAFE
  ;
operator_declarator
  : unary_operator_declarator
  | binary_operator_declarator
  | conversion_operator_declarator
  ;
unary_operator_declarator
  : type OPERATOR overloadable_unary_operator '(' type IDENTIFIER ')'
  ;
overloadable_unary_operator
  : '+' | '-' | '!' | '~' |  PLUSPLUS | MINUSMINUS | TRUE |  FALSE
  ;
binary_operator_declarator
  : type OPERATOR overloadable_binary_operator '(' type IDENTIFIER ',' type IDENTIFIER ')'
  ;
overloadable_binary_operator
  : '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' 
  | LTLT | GTGT | EQEQ | NOTEQ | '>' | '<' | GEQ | LEQ
  ;
conversion_operator_declarator
  : IMPLICIT   OPERATOR   type   '('   type   IDENTIFIER   ')'
  | EXPLICIT   OPERATOR   type   '('   type   IDENTIFIER   ')'
  ;
constructor_declaration
  : [ attributes ]   [ constructor_modifiers ]   constructor_declarator   constructor_body
  ;
constructor_modifiers
  : constructor_modifier
  | constructor_modifiers   constructor_modifier
  ;
constructor_modifier
  : PUBLIC
  | PROTECTED
  | INTERNAL
  | PRIVATE
  | EXTERN
  | UNSAFE
  ;
constructor_declarator
  : IDENTIFIER   '('   [ formal_parameter_list ]   ')'   [ constructor_initializer ]
  ;
constructor_initializer
  : ':'   BASE   '('   [ argument_list ]   ')'
  | ':'   THIS   '('   [ argument_list ]   ')'
  ;
static_constructor_declaration
  : [ attributes ]   [ UNSAFE ] STATIC   IDENTIFIER   '('   ')'   block
  ;
destructor_declaration
  : [ attributes ] [ UNSAFE ]   '~'   IDENTIFIER   '('   ')'    block
  ;
operator_body
  : block
  | ';'
  ;
constructor_body   /*** Added by JP - same as method_body ***/
  : block
  | ';'
  ;

/***** C.2.7 Structs *****/
struct_declaration
  : [ attributes ]   [ struct_modifiers ]   STRUCT   IDENTIFIER   [ struct_interfaces ]   struct_body   [ ',' ]
  ;
struct_modifiers
  : struct_modifier
  | struct_modifiers   struct_modifier
  ;
struct_modifier
  : NEW
  | PUBLIC
  | PROTECTED
  | INTERNAL
  | PRIVATE
  | UNSAFE
  ;
struct_interfaces
  : ':'   interface_type_list
  ;
struct_body
  : '{'   [ struct_member_declarations ]   '}'
  ;
struct_member_declarations
  : struct_member_declaration
  | struct_member_declarations   struct_member_declaration
  ;
struct_member_declaration
  : constant_declaration
  | field_declaration
  | method_declaration
  | property_declaration
  | event_declaration
  | indexer_declaration
  | operator_declaration
  | constructor_declaration
  | static_constructor_declaration
  | type_declaration
  ;

/***** C.2.8 Arrays *****/
array_initializer
  : '{'   [ variable_initializer_list ]   '}'
  | '{'   variable_initializer_list   ','   '}'
  ;
variable_initializer_list
  : variable_initializer
  | variable_initializer_list   ','   variable_initializer
  ;

/***** C.2.9 Interfaces *****/
interface_declaration
  : [ attributes ]   [ interface_modifiers ]   INTERFACE   IDENTIFIER   [ interface_base ]   interface_body   [ ',' ]
  ;
interface_modifiers
  : interface_modifier
  | interface_modifiers   interface_modifier
  ;
interface_modifier
  : NEW
  | PUBLIC
  | PROTECTED
  | INTERNAL
  | PRIVATE
  | UNSAFE
  ;
interface_base
  : ':'  interface_type_list
  ;
interface_body
  : '{'   [ interface_member_declarations ]   '}'
  ;
interface_member_declarations
  : interface_member_declaration
  | interface_member_declarations   interface_member_declaration
  ;
interface_member_declaration
  : interface_method_declaration
  | interface_property_declaration
  | interface_event_declaration
  | interface_indexer_declaration
  ;
interface_method_declaration
  : [ attributes ]   [ NEW ]   return_type   IDENTIFIER   '('   [ formal_parameter_list ]   ')'   ';'
  ;
interface_property_declaration
  : [ attributes ]   [ NEW ]   type   IDENTIFIER   '{'   interface_accessors   '}'
  ;
interface_accessors
  : [ attributes ]   GET   ';'
  | [ attributes ]   SET   ';'
  | [ attributes ]   GET   ';'   [ attributes ]   SET   ';'
  | [ attributes ]   SET   ';'   [ attributes ]   GET   ';'
  ;
interface_event_declaration
  : [ attributes ]   [ NEW ]   EVENT   type   IDENTIFIER   ';'
  ;
interface_indexer_declaration
  : [ attributes ]   [ NEW ]   type   THIS   '['   formal_parameter_list   ']'   '{'   interface_accessors   '}'
  ;

/***** C.2.10 Enums *****/
enum_declaration
  : [ attributes ]   [ enum_modifiers ]   ENUM   IDENTIFIER   [ enum_base ]   enum_body   [ ',' ]
  ;
enum_base
  : ':'   integral_type
  ;
enum_body
  : '{'   [ enum_member_declarations ]   '}'
  | '{'   enum_member_declarations   ','   '}'
  ;
enum_modifiers
  : enum_modifier
  | enum_modifiers   enum_modifier
  ;
enum_modifier
  : NEW
  | PUBLIC
  | PROTECTED
  | INTERNAL
  | PRIVATE
  ;
enum_member_declarations
  : enum_member_declaration
  | enum_member_declarations   ','   enum_member_declaration
  ;
enum_member_declaration
  : [ attributes ]   IDENTIFIER
  | [ attributes ]   IDENTIFIER   '='   constant_expression
  ;

/***** C.2.11 Delegates *****/
delegate_declaration
  : [ attributes ]   [ delegate_modifiers ]   DELEGATE   return_type   IDENTIFIER   '('   [ formal_parameter_list ]   ')'   ';'
  ;
delegate_modifiers
  : delegate_modifier
  | delegate_modifiers   delegate_modifier
  ;
delegate_modifier
  : NEW
  | PUBLIC
  | PROTECTED
  | INTERNAL
  | PRIVATE
  | UNSAFE
  ;

/***** C.2.12 Attributes *****/
attributes
  : attribute_sections
  ;
attribute_sections
  : attribute_section
  | attribute_sections   attribute_section
  ;
attribute_section
  : '['   [ attribute_target_specifier ]   attribute_list   ']'
  | '['   [ attribute_target_specifier ]   attribute_list   ',' ']'
  ;
attribute_target_specifier
  :  attribute_target   ':'
  ;
attribute_target
  : ASSEMBLY
  | FIELD
  | EVENT
  | METHOD
  | MODULE
  | PARAM
  | PROPERTY
  | RETURN
  | TYPE
  ;
attribute_list
  : attribute
  | attribute_list   ','   attribute
  ;
attribute
  : attribute_name   [ attribute_arguments ]
  ;
attribute_name
  : type_name
  ;
attribute_arguments
  : '('   [ positional_argument_list ]   ')'
  | '('   positional_argument_list   ','   named_argument_list   ')'
  | '('   named_argument_list   ')'
  ;
positional_argument_list
  : positional_argument
  | positional_argument_list   ','   positional_argument
  ;
positional_argument
  : attribute_argument_expression
  ;
named_argument_list
  : named_argument
  | named_argument_list   ','   named_argument
  ;
named_argument
  : IDENTIFIER   '='   attribute_argument_expression
  ;
attribute_argument_expression
  : expression
  ;

%%

