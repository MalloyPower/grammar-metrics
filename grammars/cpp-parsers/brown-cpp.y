/* Taken from version 1.26 of the the cppp C++ parser
 * Copyright 1993 Brown University -- Tony Davis.
 * See: http://www.cs.brown.edu/software/
 */


%token	<locval>	LX_ASM LX_AUTO LX_BREAK LX_CASE
%token	<locval>	LX_CATCH LX_CHAR LX_CLASS LX_CONST
%token	<locval>	LX_CONTINUE LX_DEFAULT LX_DELETE LX_DO
%token	<locval>	LX_DOUBLE LX_ENUM LX_EXTERN
%token	<locval>	LX_FLOAT LX_FOR LX_FRIEND LX_GOTO
%token	<locval>	LX_INLINE LX_INT LX_LONG
%token	<locval>	LX_NEW0 LX_OPERATOR LX_PRIVATE LX_PROTECTED
%token	<locval>	LX_PUBLIC LX_REGISTER LX_RETURN LX_SHORT
%token	<locval>	LX_SIGNED LX_STATIC LX_STRUCT
%token	<locval>	LX_SWITCH LX_TEMPLATE LX_THIS LX_THROW
%token	<locval>	LX_TRY LX_TYPEDEF LX_UNION LX_UNSIGNED
%token	<locval>	LX_VIRTUAL LX_VOID LX_VOLATILE LX_WHILE

%token	<locval>	LX_NEW

%left	<locval> ','
%right	<locval> '=' LX_MUL_EQ LX_DIV_EQ LX_MOD_EQ LX_ADD_EQ LX_SUB_EQ LX_LSH_EQ LX_RSH_EQ LX_AND_EQ LX_XOR_EQ LX_IOR_EQ
%right	<locval> '?'
%left	<locval> LX_OR_OR
%left	<locval> LX_AND_AND
%left	<locval> '|'
%left	<locval> '^'
%left	<locval> '&'
%left	<locval> LX_EQL LX_NEQ
%left	<locval> '<' '>' LX_LEQ LX_GEQ
%left	<locval> LX_LSH LX_RSH
%left	<locval> '+' '-'
%left	<locval> '*' '/' '%'
%left	<locval> LX_DOT_STAR LX_PTS_STAR
%left	<locval> LX_INCR LX_DECR LX_SIZEOF PREC_UNARY
%left	<locval> PREC_POSTFIX


%token	<locval> LX_PTS
%token	<locval> LX_COLON_COLON
%token	<locval> LX_ELLIPSES

%token <astval> LX_ID_TYPE_NAME LX_ID_ENUM_NAME LX_ID_TEMPLATE_NAME LX_ID_CONSTRUCTOR
%token <strval> LX_EXTERN_LINKAGE

%token	<locval> LX_END_TEMPLATE LX_QUAL_COLON LX_ABST_LEFTP LX_INIT_LEFTP

%token	IN_QUAL_TYPE IN_QUAL_CLASS IN_QUAL_PTR IN_QUAL_ID
%token	IN_DECLARATOR IN_CLASS_SPEC IN_ABST_DECL
%token	IN_PLACEMENT IN_FCT_DEF

%token <astval> LX_ID LX_INT_VAL LX_ZERO_VAL LX_FLT_VAL LX_STRING_VAL LX_CHAR_VAL

%left	<locval> LX_IF
%left	<locval> LX_ELSE


%token	<astval>	LX_ID0
%token	<locval>	LX_CLASS0 LX_STRUCT0 LX_UNION0


%start program


%type <astval> prog_decls
%type <astval> class_name enum_name template_name
%type <astval> opt_expression expression assignment_expression conditional_expression
%type <astval> tstd_expr cast_expression unary_expression allocation_expression
%type <astval> new_operator opt_placement new_type_name opt_new_declarator
%type <astval> new_declarator new_declarator1 new_declarator2
%type <astval> opt_new_initializer new_initializer_list deallocation_expression
%type <astval> delete_operator postfix_expression opt_expression_list expression_list
%type <astval> primary_expression expr_name name simple_name simple_expr_name
%type <astval> qualified_name qualified_expr_name literal
%type <astval> declaration decl_specifier opt_decl_specifiers decl_specifiers
%type <astval> storage_class_specifier fct_specifier type_specifier
%type <astval> simple_type_name elaborated_type_specifier
%type <astval> qualified_type_name qualified_type_name1
%type <astval> complete_class_name qualified_class_name qualified_class_name1
%type <astval> pointer_class_prefix
%type <astval> qualified_class_prefix enum_specifier opt_identifier
%type <astval> opt_enum_list enum_list enumerator
%type <astval> constant_expression linkage_specification opt_link_declaration_list
%type <astval> link_declaration_list asm_declaration
%type <astval> opt_declarator_list declarator_list init_declarator
%type <astval> opt_initializer declarator declarator1 opt_ptr_operator
%type <astval> ptr_operator opt_cv_qualifier_list cv_qualifier
%type <astval> opt_cvt_qualifier_list dname
%type <astval> type_name type_specifier_list opt_abstract_declarator
%type <astval> abstract_declarator opt_abst_declarator abst_declarator
%type <astval> abst_declarator1 abst_declarator3 abst_declarator2
%type <astval> argument_declaration_set argument_declaration_list
%type <astval> opt_arg_declaration_list arg_declaration_list
%type <astval> argument_declaration function_definition fct_body
%type <astval> initializer
%type <astval> initializer_list initializer_elt
%type <astval> class_specifier class_head opt_member_list
%type <astval> member_list member_list_elt member_declaration
%type <astval> opt_member_declarator_list
%type <astval> member_declarator_list member_declarator
%type <astval> opt_base_spec base_list base_specifier
%type <astval> virtual_specifier access_specifier conversion_function_name
%type <astval> conversion_type_name opt_ctor_initializer mem_initializer_list
%type <astval> mem_initializer operator_function_name operator
%type <astval> statement labeled_statement exprdecl_statement exprdecl
%type <astval> compound_statement opt_statement_list statement_list
%type <astval> selection_statement iteration_statement for_init_statement
%type <astval> jump_statement
%type <astval> template_declaration template_argument_list template_argument
%type <astval> template_class_name template_arg_list template_arg
%type <astval> try_block handler_list handler exception_declaration
%type <astval> throw_expression exception_specification opt_type_list
%type <astval> type_list

%type <locval> opt_comma


%token	<locval> ';' '{' '}' '(' ')' ':' '~' '!'



%%

program :	prog_decls
			
	;


prog_decls :	/* empty */
			
	|	prog_decls declaration
			
	|	prog_decls 
			
	;


/*	r.17.1	Keywords	*/


class_name :	LX_ID_TYPE_NAME
	|	template_class_name
	;


enum_name  :	LX_ID_ENUM_NAME
	;


template_name : LX_ID_TEMPLATE_NAME
	;


/*	r.17.2	Expressions	*/


opt_expression : /* empty */
			
	| expression
	;


expression :	assignment_expression
	|	expression ',' assignment_expression
			
	;


assignment_expression : conditional_expression
	|	conditional_expression '=' assignment_expression
			
	|	conditional_expression LX_MUL_EQ assignment_expression
			
	|	conditional_expression LX_DIV_EQ assignment_expression
			
	|	conditional_expression LX_MOD_EQ assignment_expression
			
	|	conditional_expression LX_ADD_EQ assignment_expression
			
	|	conditional_expression LX_SUB_EQ assignment_expression
			
	|	conditional_expression LX_RSH_EQ assignment_expression
			
	|	conditional_expression LX_LSH_EQ assignment_expression
			
	|	conditional_expression LX_AND_EQ assignment_expression
			
	|	conditional_expression LX_XOR_EQ assignment_expression
			
	|	conditional_expression LX_IOR_EQ assignment_expression
			
	;


conditional_expression : tstd_expr
	|	tstd_expr '?' expression ':' conditional_expression
			
	;


/* std_expr changed to tstd_expr to work around bug in SunOS 5.4 yacc */
tstd_expr :	tstd_expr LX_OR_OR tstd_expr
			
	|	tstd_expr LX_AND_AND tstd_expr
			
	|	tstd_expr '|' tstd_expr
			
	|	tstd_expr '^' tstd_expr
			
	|	tstd_expr '&' tstd_expr
			
	|	tstd_expr LX_EQL tstd_expr
			
	|	tstd_expr LX_NEQ tstd_expr
			
	|	tstd_expr '<' tstd_expr
			
	|	tstd_expr '>' tstd_expr
			
	|	tstd_expr LX_LEQ tstd_expr
			
	|	tstd_expr LX_GEQ tstd_expr
			
	|	tstd_expr LX_LSH tstd_expr
			
	|	tstd_expr LX_RSH tstd_expr
			
	|	tstd_expr '+' tstd_expr
			
	|	tstd_expr '-' tstd_expr
			
	|	tstd_expr '*' tstd_expr
			
	|	tstd_expr '/' tstd_expr
			
	|	tstd_expr '%' tstd_expr
			
	|	tstd_expr LX_DOT_STAR tstd_expr
			
	|	tstd_expr LX_PTS_STAR tstd_expr
			
	|	cast_expression
	;


cast_expression : unary_expression
	|	'(' type_name ')' cast_expression
			
	;


unary_expression : postfix_expression
	|	LX_INCR unary_expression
			
	|	LX_DECR unary_expression
			
	|	'*' cast_expression                     %prec PREC_UNARY
			
	|	'&' cast_expression                     %prec PREC_UNARY
			
	|	'+' cast_expression                     %prec PREC_UNARY
			
	|	'-' cast_expression                     %prec PREC_UNARY
			
	|	'!' cast_expression                     %prec PREC_UNARY
			
	|	'~' cast_expression                     %prec PREC_UNARY
			
	|	LX_SIZEOF unary_expression
			
	|	LX_SIZEOF '(' type_name ')'
			
	|	allocation_expression
	|	deallocation_expression
	|	throw_expression
	;


allocation_expression : new_operator opt_placement new_type_name
				opt_new_initializer		%prec PREC_UNARY
		
	;


new_operator :	LX_NEW0
			
	|	LX_COLON_COLON LX_NEW0
			
	;


opt_placement : /* empty */
			
	|	IN_PLACEMENT '(' expression_list ')'
			
	;


new_type_name : type_specifier_list opt_new_declarator
			
	|	'(' type_name ')'
			
	;


opt_new_declarator : /* empty */				%prec PREC_UNARY
			
	|	new_declarator					%prec PREC_UNARY
	;


new_declarator : '*' opt_cv_qualifier_list opt_new_declarator
			
	|	IN_QUAL_PTR pointer_class_prefix opt_cv_qualifier_list
				opt_new_declarator
			
	|	new_declarator1
	;


new_declarator1 : new_declarator2
	|	new_declarator1 '[' constant_expression ']'
			
	;


new_declarator2 : '[' expression ']'
			
	;


opt_new_initializer : /* empty */
			
	|	'(' ')'
			
	|	'(' new_initializer_list ')'
			
	;


new_initializer_list : assignment_expression
			
	|	new_initializer_list ',' assignment_expression
			
	|	new_initializer_list 
			
	;



deallocation_expression : delete_operator cast_expression      %prec PREC_UNARY
			
	|	delete_operator '[' ']' cast_expression        %prec PREC_UNARY
			
	;


delete_operator : LX_DELETE
			
	|	LX_COLON_COLON LX_DELETE
			
	;


postfix_expression : primary_expression
	|	postfix_expression '[' expression ']'           %prec PREC_POSTFIX
			
	|	postfix_expression '(' opt_expression_list ')'  %prec PREC_POSTFIX
			
	|	simple_type_name '(' opt_expression_list ')'    %prec PREC_POSTFIX
			
	|	postfix_expression '.' expr_name                %prec PREC_POSTFIX
			
	|	postfix_expression LX_PTS expr_name		%prec PREC_POSTFIX
			
	|	postfix_expression LX_INCR			%prec PREC_POSTFIX
			
	|	postfix_expression LX_DECR			%prec PREC_POSTFIX
			
	;


opt_expression_list : /* empty */
			
	|	expression_list
	;


expression_list : assignment_expression
			
	|	expression_list ',' assignment_expression
			
	;


primary_expression : literal
	|	LX_THIS
			
	|	LX_COLON_COLON LX_ID
			
	|	LX_COLON_COLON operator_function_name
			
	|	LX_COLON_COLON qualified_expr_name
			
	|	'(' expression ')'
			
	|	expr_name
	;


expr_name :	qualified_expr_name
	|	simple_expr_name
	;


name	:	simple_name
	|	qualified_name
	;


simple_name :	LX_ID
	|	operator_function_name
	|	conversion_function_name
	|	'~' class_name
			
	|	LX_ID_CONSTRUCTOR
			
	;


simple_expr_name : LX_ID
	|	operator_function_name
	;


qualified_name : IN_QUAL_ID qualified_class_prefix simple_name
			
	;


qualified_expr_name : IN_QUAL_ID qualified_class_prefix simple_expr_name
			
	|	IN_QUAL_ID qualified_class_prefix '~' class_name
			
	;


literal :	LX_INT_VAL
	|	LX_ZERO_VAL
	|	LX_CHAR_VAL
	|	LX_FLT_VAL
	|	LX_STRING_VAL
	;


/*	r.17.3	Declarations	*/


declaration :	opt_decl_specifiers opt_declarator_list ';'
			
	|	asm_declaration
	|	function_definition
	|	template_declaration
	|	linkage_specification
	;


decl_specifier : storage_class_specifier
	|	type_specifier
	|	fct_specifier
	|	LX_FRIEND
			
	|	LX_TYPEDEF
			
	;


opt_decl_specifiers :
			
	|	decl_specifiers
	;


decl_specifiers : decl_specifier
			
	|	decl_specifiers decl_specifier
			
	;


storage_class_specifier : LX_AUTO
			
	|	LX_REGISTER
			
	|	LX_STATIC
			
	|	LX_EXTERN
			
	;


fct_specifier : LX_INLINE
			
	|	LX_VIRTUAL
			
	;


type_specifier : simple_type_name
	|	class_specifier
	|	enum_specifier
	|	elaborated_type_specifier
	|	cv_qualifier
	;


simple_type_name : complete_class_name
	|	qualified_type_name
	|	LX_CHAR
			
	|	LX_SHORT
			
	|	LX_INT
			
	|	LX_LONG
			
	|	LX_SIGNED
			
	|	LX_UNSIGNED
			
	|	LX_FLOAT
			
	|	LX_DOUBLE
			
	|	LX_VOID
			
	;


elaborated_type_specifier : LX_CLASS LX_ID
			
	|	LX_CLASS class_name
			
	|	LX_STRUCT LX_ID
			
	|	LX_STRUCT class_name
			
	|	LX_UNION LX_ID
			
	|	LX_UNION class_name
			
	|	LX_ENUM enum_name
			
	;


qualified_type_name : enum_name
	|	IN_QUAL_TYPE class_name LX_QUAL_COLON qualified_type_name1
			
	;


qualified_type_name1 : enum_name
	|	class_name LX_QUAL_COLON qualified_type_name1
			
	;



complete_class_name : qualified_class_name
	|	LX_COLON_COLON qualified_class_name
			
	;


qualified_class_name : class_name
	|	IN_QUAL_CLASS class_name LX_QUAL_COLON qualified_class_name1
			
	;


qualified_class_name1 : class_name
	|	class_name LX_QUAL_COLON qualified_class_name1
			
	;


pointer_class_prefix : IN_QUAL_PTR qualified_class_prefix '*'
			
	|	LX_COLON_COLON IN_QUAL_PTR qualified_class_prefix '*'
			
	;


qualified_class_prefix : class_name LX_QUAL_COLON
			
	|	qualified_class_prefix class_name LX_QUAL_COLON
			
	;


enum_specifier : LX_ENUM opt_identifier
			
		   '{' opt_enum_list '}'
			
	;


opt_identifier : /* empty */
			
	|	LX_ID
	;


opt_enum_list : /* empty */
			
	|	enum_list opt_comma
	;


enum_list :	enumerator
			
	|	enum_list ',' enumerator
			
	;


enumerator :	LX_ID
			
	|	LX_ID '=' constant_expression
			
	;


constant_expression : conditional_expression
	;


linkage_specification : LX_EXTERN_LINKAGE LX_STRING_VAL '{' opt_link_declaration_list '}'
			
	|	LX_EXTERN_LINKAGE LX_STRING_VAL declaration
			
	;


opt_link_declaration_list : /* empty */
			
	|	link_declaration_list
	;


link_declaration_list : declaration
			
	|	link_declaration_list declaration
			
	|	link_declaration_list 
			
	;



asm_declaration : LX_ASM '(' LX_STRING_VAL ')' ';'
			
	;


/*	r.17.4 Declarators	*/


opt_declarator_list : /* empty */
			
	|	IN_DECLARATOR declarator_list
			
	;


declarator_list : init_declarator
			
	|	declarator_list ',' init_declarator
			
	;


init_declarator : declarator opt_initializer
			
	;


opt_initializer : /* empty */
			
	|	initializer
	;


declarator :	'*' opt_cv_qualifier_list declarator
			
	|	'&' opt_cv_qualifier_list declarator
			
	|	pointer_class_prefix opt_cv_qualifier_list declarator
			
	|	declarator1
	;


declarator1 :	declarator1 argument_declaration_set opt_cvt_qualifier_list
			
	|	declarator1 '[' constant_expression ']'
			
	|	declarator1 '[' ']'
			
	|	'(' declarator ')'
			
	|	dname
	;


opt_cv_qualifier_list : /* empty */
			
	|	opt_cv_qualifier_list cv_qualifier
			
	;


cv_qualifier :	LX_CONST
			
	|	LX_VOLATILE
			
	;


opt_cvt_qualifier_list : /* empty */
			
	|	opt_cvt_qualifier_list cv_qualifier
			
	|	opt_cvt_qualifier_list exception_specification
			
	;


dname	:	name
	|	qualified_class_name
	;


type_name :	type_specifier_list opt_abstract_declarator
			
	;


type_specifier_list : type_specifier
			
	|	type_specifier_list type_specifier
			
	;


opt_abstract_declarator : /* empty */
			
	|	abstract_declarator
	;


abstract_declarator : IN_ABST_DECL abst_declarator
			
	;


opt_abst_declarator : /* empty */
			
	|	abst_declarator
	;


abst_declarator : '*' opt_cv_qualifier_list opt_abst_declarator
			
	|	'&' opt_cv_qualifier_list opt_abst_declarator
			
	|	pointer_class_prefix opt_cv_qualifier_list opt_abst_declarator
			
	|	abst_declarator1
	;


abst_declarator1 : abst_declarator2 argument_declaration_set opt_cv_qualifier_list
			
	|	abst_declarator3
	;


abst_declarator3 : abst_declarator2 '[' constant_expression ']'
			
	|	abst_declarator2 '[' ']'
			
	|	abst_declarator3 '[' constant_expression ']'
			
	;


abst_declarator2 : /* empty */
			
	|	LX_ABST_LEFTP abst_declarator ')'
			
	;


argument_declaration_set : /* '(' */  argument_declaration_leftp
			/*  */
		    argument_declaration_list ')'
			
	|	/* '(' */  argument_declaration_leftp
			/*  */
		    LX_VOID ')'
			
	;

argument_declaration_leftp : '('
			
	;


argument_declaration_list : opt_arg_declaration_list
	|	opt_arg_declaration_list LX_ELLIPSES
			
	|	arg_declaration_list ',' LX_ELLIPSES
			
	;


opt_arg_declaration_list : /* empty */
			
	|	arg_declaration_list
	;


arg_declaration_list : argument_declaration
			
	|	arg_declaration_list ',' argument_declaration
			
	;


argument_declaration : decl_specifiers IN_DECLARATOR declarator
			
	|	decl_specifiers IN_DECLARATOR declarator '=' expression
			
	|	decl_specifiers opt_abstract_declarator
			
	|	decl_specifiers opt_abstract_declarator '=' expression
			
	;


function_definition : IN_FCT_DEF
			
		    opt_decl_specifiers IN_DECLARATOR declarator
			
		    opt_ctor_initializer fct_body
			
	;


fct_body :	'{'
			
		    opt_statement_list '}'
			
	;


initializer :	'=' assignment_expression
			
	|	'=' '{' initializer_list opt_comma '}'
			
	|	LX_INIT_LEFTP expression_list ')'
			
	;


opt_comma :	/* empty */
		
	|	','
		
	;


initializer_list : initializer_elt
			
	|	initializer_list ',' initializer_elt
			
	|	initializer_list 
			
	;


initializer_elt : assignment_expression
	|	'{' initializer_list opt_comma '}'
			
	;


/*	r.17.5	Class Declarations	*/


class_specifier : IN_CLASS_SPEC class_head '{'
			
		    opt_member_list '}'
			
	;


class_head :	LX_CLASS opt_identifier opt_base_spec
			
	|	LX_CLASS class_name opt_base_spec
			
	|	LX_STRUCT opt_identifier opt_base_spec
			
	|	LX_STRUCT class_name opt_base_spec
			
	|	LX_UNION opt_identifier opt_base_spec
			
	|	LX_UNION class_name opt_base_spec
			
	;


opt_member_list : /* empty */
			
	|	member_list
	|	
			
	;


member_list :	member_list_elt
			
	|	member_list member_list_elt
			
	|	member_list 
			
	;


member_list_elt :   member_declaration
	|	LX_PRIVATE ':'
			
	|	LX_PROTECTED ':'
			
	|	LX_PUBLIC ':'
			
	;


member_declaration : opt_decl_specifiers opt_member_declarator_list ';'
			
	|	function_definition
			
	|	qualified_name ';'
			
	;


opt_member_declarator_list : /* empty */
			
	|	IN_DECLARATOR member_declarator_list
			
	;


member_declarator_list : member_declarator
			
	|	member_declarator_list ',' member_declarator
			
	;


member_declarator : declarator
	|	declarator '=' LX_ZERO_VAL
			
	|	':' constant_expression
			
	|	LX_ID ':' constant_expression
			
	;


opt_base_spec : /* empty */
			
	|	':' base_list
			
	;


base_list :	base_specifier
			
	|	base_list ',' base_specifier
			
	;


base_specifier : complete_class_name
			
	|	virtual_specifier complete_class_name
			
	|	virtual_specifier access_specifier complete_class_name
			
	|	access_specifier complete_class_name
			
	|	access_specifier virtual_specifier complete_class_name
			
	;


virtual_specifier :  LX_VIRTUAL
			
	;


access_specifier : LX_PRIVATE
			
	|	LX_PROTECTED
			
	|	LX_PUBLIC
			
	;


conversion_function_name : LX_OPERATOR conversion_type_name
			
	;


conversion_type_name : type_specifier_list opt_ptr_operator
			
	;


opt_ptr_operator : /* empty */
			
	|	ptr_operator
	;


ptr_operator :	'*' opt_cv_qualifier_list
			
	|	'&' opt_cv_qualifier_list
			
	|	pointer_class_prefix opt_cv_qualifier_list
			
	;


opt_ctor_initializer : /* empty */
			
	|	':' mem_initializer_list
			
	|	':' 
			
	;


mem_initializer_list : mem_initializer
			
	|	mem_initializer ',' mem_initializer_list
			
	;


mem_initializer : complete_class_name '(' opt_expression_list ')'
			
	|	LX_ID '(' opt_expression_list ')'
			
	;


operator_function_name : LX_OPERATOR operator
			
	;


operator :	LX_NEW
			
	|	LX_DELETE
			
	|	'+'
			
	|	'-'
			
	|	'*'
			
	|	'/'
			
	|	'%'
			
	|	'^'
			
	|	'&'
			
	|	'|'
			
	|	'~'
			
	|	'!'
			
	|	'='
			
	|	'<'
			
	|	'>'
			
	|	LX_ADD_EQ
			
	|	LX_SUB_EQ
			
	|	LX_MUL_EQ
			
	|	LX_DIV_EQ
			
	|	LX_MOD_EQ
			
	|	LX_XOR_EQ
			
	|	LX_AND_EQ
			
	|	LX_IOR_EQ
			
	|	LX_LSH
			
	|	LX_RSH
			
	|	LX_LSH_EQ
			
	|	LX_RSH_EQ
			
	|	LX_EQL
			
	|	LX_NEQ
			
	|	LX_LEQ
			
	|	LX_GEQ
			
	|	LX_AND_AND
			
	|	LX_OR_OR
			
	|	LX_INCR
			
	|	LX_DECR
			
	|	','
			
	|	LX_PTS_STAR
			
	|	LX_PTS
			
	|	'(' ')'
			
	|	'[' ']'
			
	|	LX_DOT_STAR
			
	;


/*	r.17.6	Statements	*/


statement :	labeled_statement
	|	exprdecl_statement
	|	compound_statement
	|	selection_statement
	|	iteration_statement
	|	jump_statement
	|	try_block
	|	
			
	;


labeled_statement : LX_ID ':' statement
			
	|	LX_CASE constant_expression ':' statement
			
	|	LX_DEFAULT ':' statement
			
	;


exprdecl_statement :	exprdecl
	;


exprdecl :	expression ';'
			
	|	declaration
			
	;


compound_statement : '{'
			
		    opt_statement_list '}'
			
	;


opt_statement_list : /* empty */
			
	|	statement_list
	;


statement_list : statement
			
	|	statement_list statement
			
	;


selection_statement : LX_IF '(' expression ')' statement                %prec LX_IF
			
	|	LX_IF '(' expression ')' statement LX_ELSE statement    %prec LX_ELSE
			
	|	LX_SWITCH '(' expression ')' statement
			
	;


iteration_statement : LX_WHILE '(' expression ')' statement
			
	|	LX_DO statement LX_WHILE '(' expression ')' ';'
			
	|	LX_FOR '(' for_init_statement opt_expression ';' opt_expression ')'
				statement
			
	;


for_init_statement : exprdecl_statement
	;


jump_statement : LX_BREAK ';'
			
	|	LX_CONTINUE ';'
			
	|	LX_RETURN ';'
			
	|	LX_RETURN expression ';'
			
	|	LX_GOTO LX_ID ';'
			
	;



/*	r.17.7	Preprocessor	*/


/*	r.17.8	Templates	*/


template_declaration : LX_TEMPLATE '<' template_argument_list LX_END_TEMPLATE
			
		    declaration
			
	;


template_argument_list : template_argument
			
	|	template_argument_list ',' template_argument
			
	|	
			
	;


template_argument : argument_declaration
	;


template_class_name : template_name '<' template_arg_list LX_END_TEMPLATE
			
	;


template_arg_list : template_arg
			
	|	template_arg_list ',' template_arg
			
	;


template_arg :	assignment_expression
	|	type_name
	;


/*	r.17.9	Exception Handling	*/


try_block :	LX_TRY compound_statement handler_list
			
	;


handler_list :	handler
			
	|	handler_list handler
			
	;


handler :	LX_CATCH '(' exception_declaration ')' compound_statement
			
	;


exception_declaration : type_specifier_list IN_DECLARATOR declarator
			
	|	type_specifier_list abstract_declarator
			
	|	type_specifier_list
			
	|	LX_ELLIPSES
			
	;


throw_expression : LX_THROW				%prec PREC_UNARY
			
	|	LX_THROW cast_expression		%prec PREC_UNARY
			
	;


exception_specification : LX_THROW '(' opt_type_list ')'
			
	;


opt_type_list : /* empty */
			
	|	type_list
	;


type_list :	type_name
			
	|	type_list ',' type_name
			
	;



%%


