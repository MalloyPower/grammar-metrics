/* ANSI C++ grammar 
 * Cased on the 1998 ANSI/ISO standard (Appendix A)
 * Note: this needs to be checked and updated to bring 
 * it into line with the current (2003) ISO standard.
 */

/*** Keywords ***/
%token  asm        do           inline            short        typeid       
%token  auto       double       int               signed       typename     
%token  bool       dynamic_cast long              sizeof       union        
%token  break      else         mutable           static       unsigned     
%token  case       enum         namespace         static_cast  using        
%token  catch      explicit     new               struct       virtual      
%token  char       extern       operator          switch       void         
%token  class      false        private           template     volatile     
%token  const      float        protected         this         wchar_t      
%token  const_cast for          public            throw        while        
%token  continue   friend       register          true                       
%token  default    goto         reinterpret_cast  try                        
%token  delete     if           return            typedef   
%token  export


/*** Multi-Character Operators ***/
%token COLCOL ELLIPSES
%token PLUSEQ MINUSEQ STAREQ DIVEQ MODEQ
%token XOREQ  ANDEQ   OREQ   LTLT  GTGT     GTGTEQ     LTLTEQ     EQEQ   NOTEQ
%token LEQ    GEQ     ANDAND OROR  PLUSPLUS MINUSMINUS DOTSTAR ARROWSTAR  ARROW

/*** Literals etc. ***/
%token identifier
%token literal string_literal

%start translation_unit


%%

/********** 1.1  Keywords  [gram.key] **********/

typedef_name
  : identifier
  ;

namespace_name
  : original_namespace_name
  | namespace_alias
  ;

original_namespace_name
  : identifier
  ;

namespace_alias
  : identifier
  ;

class_name
  : identifier
  | template_id
  ;

enum_name
  : identifier
  ;

template_name
  : identifier
  ;


/**********  1.2  Lexical conventions [gram.lex] **********/


 

/**********  1.3  Basic concepts [gram.basic] **********/
translation_unit
  : [ declaration_seq ]
  ;


/**********    1.4  Expressions  [gram.expr] **********/
primary_expression
  : literal
  | this
  | '(' expression ')'
  | id_expression
  ;

id_expression
  : unqualified_id
  | qualified_id
  ;

unqualified_id
  : identifier
  | operator_function_id
  | conversion_function_id
  | '~' class_name
  | template_id
  ;
          
qualified_id
  : nested_name_specifier [ template ] unqualified_id
  | COLCOL identifier
  | COLCOL operator_function_id
  | COLCOL template_id
  ;

nested_name_specifier
  : class_or_namespace_name COLCOL [ nested_name_specifier ]
  | class_or_namespace_name COLCOL template [ nested_name_specifier ]
  ;

class_or_namespace_name
  : class_name
  | namespace_name
  ;
          
postfix_expression
  : primary_expression
  | postfix_expression '[' expression ']'
  | postfix_expression '(' [ expression_list ] ')'
  | simple_type_specifier '(' [ expression_list ] ')'
  | typename [ COLCOL ] nested_name_specifier identifier '(' [ expression_list ] ')'
  | typename [ COLCOL ] nested_name_specifier [ template ] template_id '(' [ expression_list ] ')'
  | postfix_expression '.' [ template ] [ COLCOL ] id_expression
  | postfix_expression ARROW [ template ] [ COLCOL ] id_expression
  | postfix_expression '.' pseudo_destructor_name
  | postfix_expression ARROW pseudo_destructor_name
  | postfix_expression PLUSPLUS
  | postfix_expression MINUSMINUS
  | dynamic_cast '<' type_id '>' '(' expression ')'
  | static_cast '<' type_id '>' '(' expression ')'
  | reinterpret_cast '<' type_id '>' '(' expression ')'
  | const_cast '<' type_id '>' '(' expression ')'
  | typeid '(' expression ')'
  | typeid '(' type_id ')'
  ;

          
expression_list
  : assignment_expression
  | expression_list ',' assignment_expression
  ;
          
pseudo_destructor_name
  : [ COLCOL ] [ nested_name_specifier ] type_name COLCOL '~' type_name
  | [ COLCOL ] nested_name_specifier template template_id COLCOL '~' type_name
  | [ COLCOL ] [ nested_name_specifier ] '~' type_name
  ;
          
unary_expression
  : postfix_expression
  | PLUSPLUS  cast_expression
  | MINUSMINUS  cast_expression
  | unary_operator cast_expression
  | sizeof unary_expression
  | sizeof '(' type_id ')'
  | new_expression
  | delete_expression
  ;
          
unary_operator
  : '*' | '&' | '+' | '-' | '!' | '~'
  ;
          
new_expression
  : [ COLCOL ] new [ new_placement ] new_type_id [ new_initializer ]
  | [ COLCOL ] new [ new_placement ] '(' type_id ')' [ new_initializer ]
  ;
          
new_placement
  : '(' expression_list ')'
  ;

new_type_id
  : type_specifier_seq [ new_declarator ]
  ;
          
new_declarator
  : ptr_operator [ new_declarator ]
  | direct_new_declarator
  ;

direct_new_declarator
  : '[' expression ']'
  | direct_new_declarator '[' constant_expression ']'
  ;
          
new_initializer
  : '(' [ expression_list ] ')'
  ;
          
delete_expression
  : [ COLCOL ] delete cast_expression
  | [ COLCOL ] delete '[' ']' cast_expression
  ;
          
cast_expression
  : unary_expression
  | '(' type_id ')' cast_expression
  ;
          
pm_expression
  : cast_expression
  | pm_expression DOTSTAR cast_expression
  | pm_expression ARROWSTAR cast_expression
  ;

multiplicative_expression
  : pm_expression
  | multiplicative_expression '*' pm_expression
  | multiplicative_expression '/' pm_expression
  | multiplicative_expression '%' pm_expression
  ;        

additive_expression
  : multiplicative_expression
  | additive_expression '+' multiplicative_expression
  | additive_expression '-' multiplicative_expression
  ;

shift_expression
  : additive_expression
  | shift_expression LTLT additive_expression
  | shift_expression GTGT additive_expression
  ;

relational_expression
  : shift_expression
  | relational_expression '<' shift_expression
  | relational_expression '>' shift_expression
  | relational_expression LEQ shift_expression
  | relational_expression GEQ shift_expression
  ;

equality_expression
  : relational_expression
  | equality_expression EQEQ relational_expression
  | equality_expression NOTEQ relational_expression
  ;

and_expression
  : equality_expression
  | and_expression '&' equality_expression
  ;

exclusive_or_expression
  : and_expression
  | exclusive_or_expression '^' and_expression
  ;

inclusive_or_expression
  : exclusive_or_expression
  | inclusive_or_expression '|' exclusive_or_expression
  ;

logical_and_expression
  : inclusive_or_expression
  | logical_and_expression ANDAND inclusive_or_expression
  ;

logical_or_expression
  : logical_and_expression
  | logical_or_expression OROR logical_and_expression
  ;

conditional_expression
  : logical_or_expression
  | logical_or_expression '?' expression ':' assignment_expression
  ;

assignment_expression
  : conditional_expression
  | logical_or_expression assignment_operator assignment_expression
  | throw_expression
  ;

assignment_operator
  : '=' | STAREQ | DIVEQ |  MODEQ | PLUSEQ | MINUSEQ  
  | GTGTEQ | LTLTEQ | ANDEQ | XOREQ | OREQ
  ;
          
expression
  : assignment_expression
  | expression ',' assignment_expression
  ;

constant_expression
  : conditional_expression
  ;


/**********   1.5  Statements [gram.stmt.stmt] **********/
statement
  : labeled_statement
  | expression_statement
  | compound_statement
  | selection_statement
  | iteration_statement
  | jump_statement
  | declaration_statement
  | try_block
  ;
          
labeled_statement
  : identifier ':' statement
  | case constant_expression ':' statement
  | default ':' statement
  ;

expression_statement
  : [ expression ] ';'
  ;

compound_statement
  : '{' [ statement_seq ] '}'
  ;

statement_seq
  : statement
  | statement_seq statement
  ;

selection_statement
  : if '(' condition ')' statement
  | if '(' condition ')' statement else statement
  | switch '(' condition ')' statement
  ;

condition
  : expression
  | type_specifier_seq declarator '=' assignment_expression
  ;

iteration_statement
  : while '(' condition ')' statement
  | do statement  while '(' expression ')' ';'
  | for '(' for_init_statement [ condition ] ';' [ expression ] ')' statement
  ;

for_init_statement
  : expression_statement
  | simple_declaration
  ;

jump_statement
  : break ';'
  | continue ';'
  | return [ expression ] ';'
  | goto identifier ';'
  ;

declaration_statement
  : block_declaration
  ;


/**********  1.6  Declarations  [gram.dcl.dcl] **********/

declaration_seq
  : declaration
  | declaration_seq declaration
  ;

declaration
  : block_declaration
  | function_definition
  | template_declaration
  | explicit_instantiation
  | explicit_specialization
  | linkage_specification
  | namespace_definition
  ;

block_declaration
  : simple_declaration
  | asm_definition
  | namespace_alias_definition
  | using_declaration
  | using_directive
  ;

simple_declaration
  : [ decl_specifier_seq ] [ init_declarator_list ] ';'
  ;


decl_specifier
  : storage_class_specifier
  | type_specifier
  | function_specifier
  | friend
  | typedef
  ;

decl_specifier_seq
  : [ decl_specifier_seq ] decl_specifier
  ;

storage_class_specifier
  : auto
  | register
  | static
  | extern
  | mutable
  ;

function_specifier
  : inline
  | virtual
  | explicit
  ;


type_specifier
  : simple_type_specifier
  | class_specifier
  | enum_specifier
  | elaborated_type_specifier
  | cv_qualifier
  ;

simple_type_specifier
  : [ COLCOL ] [ nested_name_specifier ] type_name
  | char
  | wchar_t
  | bool
  | short
  | int
  | long
  | signed
  | unsigned
  | float
  | double
  | void
  ;

type_name
  : class_name
  | enum_name
  | typedef_name
  ;

elaborated_type_specifier
  : class_key [ COLCOL ] [ nested_name_specifier ] identifier
  | enum [ COLCOL ] [ nested_name_specifier ] identifier
  | typename [ COLCOL ] nested_name_specifier identifier
  | typename [ COLCOL ] nested_name_specifier [ template ] template_id
  ;

enum_specifier
  : enum [ identifier ] '{' [ enumerator_list ] '}'
  ;


enumerator_list
  : enumerator_definition
  | enumerator_list ',' enumerator_definition
  ;

enumerator_definition
  : enumerator
  | enumerator '=' constant_expression
  ;

enumerator
  : identifier
  ;


namespace_definition
  : named_namespace_definition
  | unnamed_namespace_definition
  ;


named_namespace_definition
  : original_namespace_definition
  | extension_namespace_definition
  ;


original_namespace_definition
  : namespace identifier '{' namespace_body '}'
  ;


extension_namespace_definition
  : namespace original_namespace_name  '{' namespace_body '}'
  ;


unnamed_namespace_definition
  : namespace '{' namespace_body '}'
  ;


namespace_body
  : [ declaration_seq ]
  ;


namespace_alias_definition
  : namespace identifier '=' qualified_namespace_specifier ';'
  ;


qualified_namespace_specifier
  : [ COLCOL ] [ nested_name_specifier ] namespace_name
  ;

using_declaration
  : using [ typename ] [ COLCOL ] nested_name_specifier unqualified_id ';'
  | using COLCOL  unqualified_id ';'
  ;

using_directive
  : using  namespace [ COLCOL ] [ nested_name_specifier ] namespace_name ';'
  ;

asm_definition
  : asm '(' string_literal ')' ';'
  ;

linkage_specification
  : extern string_literal '{' [ declaration_seq ] '}'
  | extern string_literal declaration
  ;



/**********  1.7  Declarators [gram.dcl.decl] **********/

init_declarator_list
  : init_declarator
  | init_declarator_list ',' init_declarator
  ;

init_declarator
  : declarator [ initializer ]
  ;

declarator
  : direct_declarator
  | ptr_operator declarator
  ;

direct_declarator
  : declarator_id
  | direct_declarator '(' parameter_declaration_clause ')' [ cv_qualifier_seq ] [ exception_specification ]
  | direct_declarator '[' [ constant_expression ] ']'
  | '(' declarator ')'
  ;

ptr_operator
  : '*' [ cv_qualifier_seq ]
  | '&'
  | [ COLCOL ] nested_name_specifier '*' [ cv_qualifier_seq ]
  ;

cv_qualifier_seq
  : cv_qualifier [ cv_qualifier_seq ]
  ;

cv_qualifier
  : const
  | volatile
  ;

declarator_id
  : [ COLCOL ] id_expression
  | [ COLCOL ] [ nested_name_specifier ] type_name
  ;

type_id
  : type_specifier_seq [ abstract_declarator ]
  ;

type_specifier_seq
  : type_specifier [ type_specifier_seq ]
  ;

abstract_declarator
  : ptr_operator [ abstract_declarator ]
  | direct_abstract_declarator
  ;

direct_abstract_declarator
  : [ direct_abstract_declarator ] '(' parameter_declaration_clause ')' [ cv_qualifier_seq ] [ exception_specification ]
  | [ direct_abstract_declarator ] '[' [ constant_expression ] ']'
  | '(' abstract_declarator ')'
  ;

parameter_declaration_clause
  : [ parameter_declaration_list ] [ ELLIPSES ]
  | parameter_declaration_list ',' ELLIPSES
  ;

parameter_declaration_list
  : parameter_declaration
  | parameter_declaration_list ',' parameter_declaration
  ;

parameter_declaration
  : decl_specifier_seq declarator
  | decl_specifier_seq declarator '=' assignment_expression
  | decl_specifier_seq [ abstract_declarator ]
  | decl_specifier_seq [ abstract_declarator ] '=' assignment_expression
  ;

function_definition
  : [ decl_specifier_seq ] declarator [ ctor_initializer ] function_body
  | [ decl_specifier_seq ] declarator function_try_block
  ;

function_body
  : compound_statement
  ;

initializer
  : '=' initializer_clause
  | '(' expression_list ')'
  ;

initializer_clause
  : assignment_expression
  | '{' initializer_list [ ',' ] '}'
  | '{' '}'
  ;

initializer_list
  : initializer_clause
  | initializer_list ',' initializer_clause
  ;

/**********  1.8  Classes [gram.class] **********/
class_specifier
  : class_head '{' [ member_specification ] '}'
  ;

class_head
  : class_key [ identifier ] [ base_clause ]
  | class_key nested_name_specifier identifier [ base_clause ]
  ;

class_key
  : class
  | struct
  | union
  ;

member_specification
  : member_declaration [ member_specification ]
  | access_specifier ':' [ member_specification ]
  ;

member_declaration
  : [ decl_specifier_seq ] [ member_declarator_list ] ';'
  | function_definition [ ';' ]
  | [ COLCOL ] nested_name_specifier [ template ] unqualified_id ';'
  | using_declaration
  | template_declaration
  ;

member_declarator_list
  : member_declarator
  | member_declarator_list ',' member_declarator
  ;

member_declarator
  : declarator [ pure_specifier ]
  | declarator [ constant_initializer ]
  | [ identifier ] ':' constant_expression
  ;

pure_specifier
  : '=' '0'
  ;

constant_initializer
  : '=' constant_expression
  ;

/**********  1.9  Derived classes [gram.class.derived] **********/
base_clause
  : ':' base_specifier_list
  ;

base_specifier_list
  : base_specifier
  | base_specifier_list ',' base_specifier
  ;

base_specifier
  : [ COLCOL ] [ nested_name_specifier ] class_name
  | virtual [ access_specifier ] [ COLCOL ] [ nested_name_specifier ] class_name
  | access_specifier [ virtual ] [ COLCOL ] [ nested_name_specifier ] class_name
  ;

access_specifier
  : private
  | protected
  | public
  ;

/**********  1.10  Special member functions [gram.special] **********/
conversion_function_id
  : operator conversion_type_id
  ;

conversion_type_id
  : type_specifier_seq [ conversion_declarator ]
  ;

conversion_declarator
  : ptr_operator [ conversion_declarator ]
  ;

ctor_initializer
  : ':' mem_initializer_list
  ;

mem_initializer_list
  : mem_initializer
  | mem_initializer ',' mem_initializer_list
  ;

mem_initializer
  : mem_initializer_id '(' [ expression_list ] ')'
  ;

mem_initializer_id
  : [ COLCOL ] [ nested_name_specifier ] class_name
  | identifier
  ;


/**********  1.11  Overloading [gram.over] **********/
operator_function_id
  : operator an_operator
  ;        

an_operator
  :  new   | delete   |   new '[' ']'   |    delete '[' ']'
  | '+'   |   '-'  |    '*'  |    '/'   |   '%'   
  |   '^'  |    '&'   |   '|'  |    '~'
  |  '!'    '='   |   '<'  |    '>'   
  |   PLUSEQ  |   MINUSEQ  |   STAREQ  |   DIVEQ  |   MODEQ
  |  XOREQ   |  ANDEQ  |   OREQ  
  |   LTLT  |   GTGT   |  GTGTEQ  |  LTLTEQ  
  |  EQEQ   NOTEQ |  LEQ  |   GEQ  
  |  ANDAND  |   OROR |    PLUSPLUS  |   MINUSMINUS  
  |   ','   |   ARROWSTAR |   ARROW
  |  '(' ')'  |   '[' ']'
  ;

/**********  1.12  Templates [gram.temp] **********/
template_declaration
  : [ export ] template '<' template_parameter_list '>' declaration
  ;

template_parameter_list
  : template_parameter
  | template_parameter_list ',' template_parameter
  ;

template_parameter
  : type_parameter
  | parameter_declaration
  ;

type_parameter
  : class [ identifier ]
  | class [ identifier ] '=' type_id
  | typename [ identifier ]
  | typename [ identifier ] '=' type_id
  | template '<' template_parameter_list '>' class  [ identifier ]
  | template '<' template_parameter_list '>' class  [ identifier ] '=' id_expression
  ;

template_id
  : template_name '<' template_argument_list '>'
  ;


template_argument_list
  : template_argument
  | template_argument_list ',' template_argument
  ;

template_argument
  : assignment_expression
  | type_id
  | id_expression
  ;

explicit_instantiation
  : template declaration
  ;

explicit_specialization
  : template '<' '>' declaration
  ;


/**********  1.13  Exception handling [gram.except] **********/
try_block
  : try compound_statement handler_seq
  ;

function_try_block
  : try  [ ctor_initializer ] function_body handler_seq
  ;

handler_seq
  : handler [ handler_seq ]
  ;

handler
  : catch '(' exception_declaration ')' compound_statement
  ;

exception_declaration
  : type_specifier_seq declarator
  | type_specifier_seq abstract_declarator
  | type_specifier_seq
  | ELLIPSES
  ;

throw_expression
  : throw [ assignment_expression ]
  ;

exception_specification
  : throw '(' [ type_id_list ] ')'
  ;

type_id_list
  : type_id
  | type_id_list ','  type_id
  ;

/**********  1.14  Preprocessing directives [gram.cpp] **********/


%%
