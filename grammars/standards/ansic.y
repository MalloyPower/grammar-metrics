/* ANSI C grammar
 * Based on the ANSI/ISO standard,
 * as in 2nd edition of K&R (not 'old-style' K&R)
 */


%token IDENTIFIER STRING_LITERAL SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN TYPEDEF_NAME

%token INTEGER_CONSTANT CHARACTER_CONSTANT FLOATING_CONSTANT ENUMERATION_CONSTANT

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token STRUCT UNION ENUM ELIPSIS RANGE

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN


%%

translation_unit
	: external_declaration
	| translation_unit external_declaration
	;

external_declaration
	: function_definition
	| declaration
	;

function_definition
	: [ declaration_specifiers ] declarator [ declaration_list ] compound_statement
	;

declaration
	: declaration_specifiers [ init_declarator_list ] ';'
	;

declaration_list
	: declaration
	| declaration_list declaration
	;

declaration_specifiers
	: storage_class_specifier [ declaration_specifiers ]
	| type_specifier [ declaration_specifiers ]
	| type_qualifier [ declaration_specifiers ]
	;

storage_class_specifier
	: AUTO
	| REGISTER
	| STATIC
	| EXTERN
	| TYPEDEF
	;

type_specifier
	: VOID
	| CHAR
	| SHORT
	| INT
	| LONG
	| FLOAT
	| DOUBLE
	| SIGNED
	| UNSIGNED
	| struct_or_union_specifier
	| enum_specifier
	| TYPEDEF_NAME
	;

type_qualifier
	: CONST
	| VOLATILE
        ;

struct_or_union_specifier
	: struct_or_union [ IDENTIFIER ] '{' struct_declaration_list '}'
	| struct_or_union IDENTIFIER
	;

struct_or_union
	: STRUCT
	| UNION
	;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration
	;

init_declarator_list
	: init_declarator
	| init_declarator_list ',' init_declarator
	;

init_declarator
	: declarator
	| declarator '=' initializer
	;

struct_declaration
	: specifier_qualifier_list struct_declarator_list ';'
	;

specifier_qualifier_list 
	: type_specifier [ specifier_qualifier_list ]
	| type_qualifier [ specifier_qualifier_list ]
	;

struct_declarator_list
	: struct_declarator
	| struct_declarator_list ',' struct_declarator
	;

struct_declarator
	: declarator
	| [ declarator ] ':' constant_expression
	;

enum_specifier
	: ENUM [ IDENTIFIER ] '{' enumerator_list '}'
	| ENUM IDENTIFIER
	;

enumerator_list
	: enumerator
	| enumerator_list ',' enumerator
	;

enumerator
	: IDENTIFIER
	| IDENTIFIER '=' constant_expression
	;

declarator
	: [ pointer ] direct_declarator
	;

direct_declarator
	: IDENTIFIER
	| '(' declarator ')'
	| direct_declarator '[' [ constant_expression ] ']'
	| direct_declarator '(' parameter_type_list ')'
	| direct_declarator '(' [ identifier_list ] ')'
	;

pointer
	: '*' [ type_qualifier_list ]
	| '*' [ type_qualifier_list ] pointer
	;

type_qualifier_list
	: type_qualifier
	| type_qualifier_list type_qualifier
	;

parameter_type_list
	: parameter_list
	| parameter_list ',' ELIPSIS
	;

parameter_list
	: parameter_declaration
	| parameter_list ',' parameter_declaration
        ;	

parameter_declaration
	: declaration_specifiers declarator
	| declaration_specifiers [ abstract_declarator ]
	;

identifier_list
	: IDENTIFIER
	| identifier_list ',' IDENTIFIER
	;

initializer
	: assignment_expression
	| '{' initializer_list '}'
	| '{' initializer_list ',' '}'
	;

initializer_list
	: initializer
	| initializer_list ',' initializer
	;


type_name
	: specifier_qualifier_list [ abstract_declarator ]
	;

abstract_declarator
	: pointer
	| [ pointer ] direct_abstract_declarator
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'
	| [ direct_abstract_declarator ] '[' [ constant_expression ] ']'
	| [ direct_abstract_declarator ] '(' [ parameter_type_list ] ')'
	;

typedef_name
	: IDENTIFIER
        ;

statement
	: labeled_statement
	| expression_statement
	| compound_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	;

labeled_statement
	: IDENTIFIER ':' statement
	| CASE constant_expression ':' statement
	| DEFAULT ':' statement
	;

expression_statement
	: [ expression ] ';'
	;

compound_statement
	: '{' [ declaration_list ] [ statement_list ] '}'
	;

statement_list
	: statement
	| statement_list statement
	;

selection_statement
	: IF '(' expression ')' statement
	| IF '(' expression ')' statement ELSE statement
	| SWITCH '(' expression ')' statement
	;

iteration_statement
	: WHILE '(' expression ')' statement
	| DO statement WHILE '(' expression ')' ';'
	| FOR '(' [ expression ] ';' [ expression ] ';' [ expression ] ')' statement
	;

jump_statement
	: GOTO IDENTIFIER ';'
	| CONTINUE ';'
	| BREAK ';'
	| RETURN [ expression ] ';'
	;

expression
	: assignment_expression
	| expression ',' assignment_expression
	;

assignment_expression
	: conditional_expression
	| unary_expression assignment_operator assignment_expression
	;

assignment_operator
	: '='
	| MUL_ASSIGN
	| DIV_ASSIGN
	| MOD_ASSIGN
	| ADD_ASSIGN
	| SUB_ASSIGN
	| LEFT_ASSIGN
	| RIGHT_ASSIGN
	| AND_ASSIGN
	| XOR_ASSIGN
	| OR_ASSIGN
	;

conditional_expression
	: logical_or_expression
	| logical_or_expression '?' expression ':' conditional_expression
	;

constant_expression
	: conditional_expression
	;

logical_or_expression
	: logical_and_expression
	| logical_or_expression OR_OP logical_and_expression
	;

logical_and_expression
	: inclusive_or_expression
	| logical_and_expression AND_OP inclusive_or_expression
	;

inclusive_or_expression
	: exclusive_or_expression
	| inclusive_or_expression '|' exclusive_or_expression
	;

exclusive_or_expression
	: and_expression
	| exclusive_or_expression '^' and_expression
	;

and_expression
	: equality_expression
	| and_expression '&' equality_expression
	;

equality_expression
	: relational_expression
	| equality_expression EQ_OP relational_expression
	| equality_expression NE_OP relational_expression
	;

relational_expression
	: shift_expression
	| relational_expression '<' shift_expression
	| relational_expression '>' shift_expression
	| relational_expression LE_OP shift_expression
	| relational_expression GE_OP shift_expression
	;

shift_expression
	: additive_expression
	| shift_expression LEFT_OP additive_expression
	| shift_expression RIGHT_OP additive_expression
	;

additive_expression
	: multiplicative_expression
	| additive_expression '+' multiplicative_expression
	| additive_expression '-' multiplicative_expression
	;

multiplicative_expression
	: cast_expression
	| multiplicative_expression '*' cast_expression
	| multiplicative_expression '/' cast_expression
	| multiplicative_expression '%' cast_expression
	;

cast_expression
	: unary_expression
	| '(' type_name ')' cast_expression
	;

unary_expression
	: postfix_expression
	| INC_OP unary_expression
	| DEC_OP unary_expression
	| unary_operator cast_expression
	| SIZEOF unary_expression
	| SIZEOF '(' type_name ')'
	;

unary_operator
	: '&'
	| '*'
	| '+'
	| '-'
	| '~'
	| '!'
	;

postfix_expression
	: primary_expression
	| postfix_expression '[' expression ']'
	| postfix_expression '(' [ argument_expression_list ] ')'
	| postfix_expression '.' IDENTIFIER
	| postfix_expression PTR_OP IDENTIFIER
	| postfix_expression INC_OP
	| postfix_expression DEC_OP
	;

primary_expression
	: IDENTIFIER
	| constant
	| STRING_LITERAL
	| '(' expression ')'
	;

argument_expression_list
	: assignment_expression
	| argument_expression_list ',' assignment_expression
	;

constant
	: INTEGER_CONSTANT
	| CHARACTER_CONSTANT
	| FLOATING_CONSTANT
	| ENUMERATION_CONSTANT
	;

%%

