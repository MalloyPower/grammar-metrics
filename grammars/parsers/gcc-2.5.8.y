/* YACC parser for C++ syntax.
   Copyright (C) 1988, 1989, 1993 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This grammar is based on the GNU CC grammar.  */

/* Note: Bison automatically applies a default action of "$$ = $1" for
   all derivations; this is applied before the explicit action, if one
   is given.  Keep this in mind when reading the actions.  */

/* Also note: this version contains experimental exception
   handling features.  They could break, change, disappear,
   or otherwise exhibit volatile behavior.  Don't depend on
   me (Michael Tiemann) to protect you from any negative impact
   this may have on your professional, personal, or spiritual life.

   NEWS FLASH:  This version now supports the exception handling
   syntax of Stroustrup's 2nd edition, if -fansi-exceptions is given.
   THIS IS WORK IN PROGRESS!!!  The type of the 'throw' and the
   'catch' much match EXACTLY (no inheritance support or coercions).
   Also, throw-specifications of functions don't work.
   Destructors aren't called correctly.  Etc, etc.  --Per Bothner.
  */



%start program

 
/* All identifiers that are not reserved words
   and are not declared typedefs in the current block */
%token IDENTIFIER

/* All identifiers that are declared typedefs in the current block.
   In some contexts, they are treated just like IDENTIFIER,
   but they can also serve as typespecs in declarations.  */
%token TYPENAME

/* Qualified identifiers that end in a TYPENAME.  */
%token SCOPED_TYPENAME

/* Reserved words that specify storage class.
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token SCSPEC

/* Reserved words that specify type.
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token TYPESPEC

/* Reserved words that qualify type: "const" or "volatile".
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token TYPE_QUAL

/* Character or numeric constants.
   yylval is the node for the constant.  */
%token CONSTANT

/* String constants in raw form.
   yylval is a STRING_CST node.  */
%token STRING

/* "...", used for functions with variable arglists.  */
%token ELLIPSIS

/* the reserved words */
/* SCO include files test "ASM", so use something else. */
%token SIZEOF ENUM /* STRUCT UNION */ IF ELSE WHILE DO FOR SWITCH CASE DEFAULT
%token BREAK CONTINUE RETURN GOTO ASM_KEYWORD GCC_ASM_KEYWORD TYPEOF ALIGNOF
%token HEADOF CLASSOF
%token ATTRIBUTE EXTENSION LABEL

/* the reserved words... C++ extensions */
%token <ttype> AGGR
%token <itype> VISSPEC
%token DELETE NEW OVERLOAD THIS OPERATOR
%token LEFT_RIGHT TEMPLATE
%token TYPEID DYNAMIC_CAST
%token <itype> SCOPE

/* Special token created by the lexer to separate TYPENAME
   from an ABSDCL.  This allows us to parse `foo (*pf)()'.  */

%token START_DECLARATOR

/* Define the operator tokens and their precedences.
   The value is an integer because, if used, it is the tree code
   to use in the expression made from the operator.  */

%left EMPTY			/* used to resolve s/r with epsilon */

/* Add precedence rules to solve dangling else s/r conflict */
%nonassoc IF
%nonassoc ELSE

%left IDENTIFIER TYPENAME TYPENAME_COLON SCSPEC TYPESPEC TYPE_QUAL ENUM AGGR

%left '{' ','

%right <code> ASSIGN '='
%right <code> '?' ':' RANGE
%left <code> OROR
%left <code> ANDAND
%left <code> '|'
%left <code> '^'
%left <code> '&'
%left <code> MIN_MAX
%left <code> EQCOMPARE
%left <code> ARITHCOMPARE '<' '>'
%left <code> LSHIFT RSHIFT
%left <code> '+' '-'
%left <code> '*' '/' '%'
%right <code> UNARY PLUSPLUS MINUSMINUS
%left HYPERUNARY
%left <ttype> PAREN_STAR_PAREN LEFT_RIGHT
%left <code> POINTSAT POINTSAT_STAR '.' DOT_STAR '(' '['

%right SCOPE			/* C++ extension */
%nonassoc NEW DELETE RAISE RAISES RERAISE TRY EXCEPT CATCH THROW
%nonassoc ANSI_TRY ANSI_THROW

%type <code> unop

%type <ttype> identifier IDENTIFIER TYPENAME CONSTANT expr nonnull_exprlist
%type <ttype> optional_identifier paren_expr_or_null
%type <ttype> expr_no_commas cast_expr unary_expr primary string STRING
%type <ttype> typed_declspecs reserved_declspecs
%type <ttype> typed_typespecs reserved_typespecquals
%type <ttype> declmods typespec typespecqual_reserved
%type <ttype> SCSPEC TYPESPEC TYPE_QUAL nonempty_type_quals maybe_type_qual
%type <itype> initdecls notype_initdecls initdcl	/* C++ modification */
%type <ttype> init initlist maybeasm
%type <ttype> asm_operands nonnull_asm_operands asm_operand asm_clobbers
%type <ttype> maybe_attribute attribute_list attrib
%type <ttype> abs_member_declarator after_type_member_declarator

%type <ttype> compstmt except_stmts ansi_except_stmts implicitly_scoped_stmt

%type <ttype> declarator notype_declarator after_type_declarator

%type <ttype> structsp opt.component_decl_list component_decl_list
%type <ttype> component_decl components component_declarator
%type <ttype> enumlist enumerator
%type <ttype> typename absdcl absdcl1 type_quals abs_or_notype_decl
%type <ttype> xexpr see_typename parmlist parms parm bad_parm
%type <ttype> identifiers_or_typenames

/* C++ extensions */
%type <ttype> typename_scope
%token <ttype> TYPENAME_COLON TYPENAME_ELLIPSIS
%token <ttype> PTYPENAME SCOPED_TYPENAME
%token <ttype> PRE_PARSED_FUNCTION_DECL EXTERN_LANG_STRING ALL
%token <ttype> PRE_PARSED_CLASS_DECL
%type <ttype> fn.def1 /* Not really! */
%type <ttype> fn.def2 return_id
%type <ttype> named_class_head named_class_head_sans_basetype
%type <ttype> unnamed_class_head
%type <ttype> class_head base_class_list
%type <itype> base_class_visibility_list
%type <ttype> base_class maybe_base_class_list base_class.1
%type <ttype> after_type_declarator_no_typename
%type <ttype> maybe_raises raise_identifier raise_identifiers ansi_raise_identifier ansi_raise_identifiers
%type <ttype> component_declarator0 id_scope scoped_typename scoped_base_class
%type <ttype> forhead.1 identifier_or_opname operator_name
%type <ttype> new delete object object_star aggr
/* %type <ttype> primary_no_id */
%type <ttype> nonmomentary_expr
%type <itype> forhead.2 initdcl0 notype_initdcl0 member_init_list
%type <itype> .scope try ansi_try
%type <ttype> template_header template_parm_list template_parm
%type <ttype> template_type template_arg_list template_arg
%type <ttype> template_instantiation template_type_name tmpl.1 tmpl.2
%type <ttype> template_instantiate_once template_instantiate_some
%type <itype> fn_tmpl_end
/* %type <itype> try_for_typename */
%type <ttype> condition partially_scoped_stmt xcond paren_cond_or_null
%type <strtype> .kindof_pushlevel

/* in order to recognize aggr tags as defining and thus shadowing. */
%token TYPENAME_DEFN IDENTIFIER_DEFN PTYPENAME_DEFN
%type <ttype> named_class_head_sans_basetype_defn 
%type <ttype> identifier_defn IDENTIFIER_DEFN TYPENAME_DEFN PTYPENAME_DEFN

%type <strtype> .pushlevel

/* cp-spew.c depends on this being the last token.  Define
   any new tokens before this one!  */
%token END_OF_SAVED_INPUT


%left error
%%
program: /* empty */
	| extdefs
			;

/* the reason for the strange actions in this rule
 is so that notype_initdecls when reached via datadef
 can find a valid list of type and sc specs in $0. */

extdefs:
	   extdef
			| extdefs extdef
			;

.hush_warning:
			;
.warning_ok:
			;

asm_keyword:
	ASM_KEYWORD 	| GCC_ASM_KEYWORD
	;

extdef:
	  fndef
			| datadef
			| template_def
			| overloaddef
	| asm_keyword '(' string ')' ';'
			| extern_lang_string '{' extdefs '}'
			| extern_lang_string '{' '}'
			| extern_lang_string .hush_warning fndef .warning_ok
			| extern_lang_string .hush_warning datadef .warning_ok
			;

extern_lang_string:
	  EXTERN_LANG_STRING
			;

template_header:
	  TEMPLATE '<'
			  template_parm_list '>'
			;

template_parm_list:
	  template_parm
			| template_parm_list ',' template_parm
			;

template_parm:
	/* The following rules introduce a new reduce/reduce
	   conflict: they are valid prefixes for a `structsp',
	   which means they could match a nameless parameter.
	   By putting them before the `parm' rule, we get
	   their match before considering them nameless parameter
	   declarations.  */
	  aggr identifier
			| aggr identifier_defn ':' base_class.1
			| aggr TYPENAME_COLON base_class.1
			| parm
	;

overloaddef:
	  OVERLOAD ov_identifiers ';'
			;

ov_identifiers: IDENTIFIER
			| ov_identifiers ',' IDENTIFIER
			;
	  
template_def:
	/* Class template declarations go here; they aren't normal class
	   declarations, because we can't process the bodies yet.  */
	  template_header named_class_head_sans_basetype '{'
		'{'	 ';'
	| template_header named_class_head_sans_basetype_defn '{'
		'{'	 ';'
	| template_header named_class_head_sans_basetype ':'
			 ';'
	| template_header named_class_head_sans_basetype_defn ':'
			  ';'
	| template_header named_class_head_sans_basetype ';'
			| template_header named_class_head_sans_basetype_defn ';'
			| template_header /* notype_initdcl0 ';' */
	  notype_declarator maybe_raises maybeasm maybe_attribute
	  fn_tmpl_end
			| template_header typed_declspecs /*initdcl0*/
	  declarator maybe_raises maybeasm maybe_attribute
	  fn_tmpl_end
			| template_header declmods declarator fn_tmpl_end
			/* Try to recover from syntax errors in templates.  */
	| template_header error '}'		| template_header error ';'		;

fn_tmpl_end: '{'		'{'	| ':'				| ';'				| '='				| RETURN			;

datadef:
	  notype_initdecls ';'
			| declmods notype_initdecls ';'
			/* Normal case to make fast: "int i;".  */
	| declmods declarator ';'
			| typed_declspecs initdecls ';'
			/* Normal case: make this fast.  */
	| typed_declspecs declarator ';'
		        | declmods ';'
	  	| typed_declspecs ';'
	  	| error ';'
	| error '}'
	| ';'
	;

fndef:
	  fn.def1 base_init compstmt_or_error
			| fn.def1 return_init base_init compstmt_or_error
			| fn.def1 nodecls compstmt_or_error
			| fn.def1 return_init ';' nodecls compstmt_or_error
			| fn.def1 return_init nodecls compstmt_or_error
			| typed_declspecs declarator error
			| declmods notype_declarator error
			| notype_declarator error
			;

fn.def1:
	  typed_declspecs declarator maybe_raises
			| declmods notype_declarator maybe_raises
			| notype_declarator maybe_raises
			| TYPENAME '(' parmlist ')' type_quals maybe_raises
			| scoped_typename '(' parmlist ')' type_quals maybe_raises
			| TYPENAME LEFT_RIGHT type_quals maybe_raises
			| scoped_typename LEFT_RIGHT type_quals maybe_raises
			| PRE_PARSED_FUNCTION_DECL
			;

/* more C++ complexity */
fn.def2:
	  typed_declspecs '(' parmlist ')' type_quals maybe_raises
			| typed_declspecs LEFT_RIGHT type_quals maybe_raises
			| typed_declspecs declarator maybe_raises
			| declmods '(' parmlist ')' type_quals maybe_raises
			| declmods LEFT_RIGHT type_quals maybe_raises
			| declmods declarator maybe_raises
			| notype_declarator maybe_raises
			;

return_id: RETURN IDENTIFIER
			;

return_init: return_id
			| return_id '=' init
			| return_id '(' nonnull_exprlist ')'
			| return_id LEFT_RIGHT
			;

base_init:
	  ':' .set_base_init member_init_list
			;

.set_base_init:
	/* empty */
			;

member_init_list:
	  /* empty */
			| member_init
			| member_init_list ',' member_init
	| member_init_list error
	;

member_init: '(' nonnull_exprlist ')'
			| LEFT_RIGHT
			| identifier '(' nonnull_exprlist ')'
			| identifier LEFT_RIGHT
			| template_type_name '(' nonnull_exprlist ')'
			| template_type_name LEFT_RIGHT
			| scoped_typename '(' nonnull_exprlist ')'
			| scoped_typename LEFT_RIGHT
			| id_scope identifier '(' nonnull_exprlist ')'
			| id_scope identifier LEFT_RIGHT
			;

identifier:
	  IDENTIFIER
	| TYPENAME
	| PTYPENAME
	;

identifier_defn:
	  IDENTIFIER_DEFN
	| TYPENAME_DEFN
	| PTYPENAME_DEFN
	;

identifier_or_opname:
	  IDENTIFIER
	| TYPENAME
	| PTYPENAME
/*	| '~' TYPENAME
		*/
	/* get rid of the next line, replace it with the above */
	| '~' identifier 	| operator_name
	;

template_type:
	  template_type_name tmpl.1 template_instantiation
			;

template_type_name:
	  PTYPENAME '<' template_arg_list '>'
			| TYPENAME  '<' template_arg_list '>'
			;

tmpl.1:
	/* Expansion of template may be required, unless we're followed by
	   a class definition.  */
	  '{'	'{'	| ':'		| /* empty */ %prec EMPTY
                	;

tmpl.2:
	/* Always do expansion if it hasn't been done already. */
			;

template_arg_list:
	  template_arg
			| template_arg_list ',' template_arg
			;

template_arg:
	  typename
			| expr_no_commas  %prec UNARY
	;

template_instantiate_once:
	  PRE_PARSED_CLASS_DECL maybe_base_class_list
			  left_curly opt.component_decl_list '}'
			;

template_instantiation:
          /* empty */
                        | template_instantiate_once
                        ;

template_instantiate_some:
          /* empty */
                        | template_instantiate_once template_instantiate_some
                        ;

unop:     '-'
			| '+'
			| PLUSPLUS
			| MINUSMINUS
			| '!'
			;

expr:	  nonnull_exprlist
			/* Ugly, but faster.  */
	| expr_no_commas
	;

paren_expr_or_null:
	LEFT_RIGHT
			| '(' expr ')'
			;

paren_cond_or_null:
	LEFT_RIGHT
			| '(' condition ')'
			;

xcond:
	/* empty */
			| condition
	| error
			;

condition:
	typed_typespecs declarator maybe_raises maybeasm maybe_attribute '='
			init
			| expr
	;

/* Used for the blocks controlled by a condition, to add any DECLs in
   the condition to the controlled block.  */
.kindof_pushlevel: /* empty */
		    
	;

/* Like implicitly_scoped_stmt, but uses .kindof_pushlevel */
partially_scoped_stmt:
           '{' .kindof_pushlevel '}'
			| '{' .kindof_pushlevel maybe_label_decls stmts '}'
			| '{' .kindof_pushlevel maybe_label_decls error '}'
			| .kindof_pushlevel simple_stmt
			;

already_scoped_stmt:
          '{' '}'
			| '{' maybe_label_decls stmts '}'
			| '{' maybe_label_decls error '}'
			| simple_stmt
	;

nonnull_exprlist:
	  expr_no_commas
			| nonnull_exprlist ',' expr_no_commas
			| nonnull_exprlist ',' error
			;

unary_expr:
	  primary %prec UNARY
			/* __extension__ turns off -pedantic for following primary.  */
	| EXTENSION
			  cast_expr	  %prec UNARY
			| '*' cast_expr   %prec UNARY
			| '&' cast_expr   %prec UNARY
			| '~' cast_expr   %prec UNARY
			| unop cast_expr  %prec UNARY
			/* Refer to the address of a label as a pointer.  */
	| ANDAND identifier
			| SIZEOF unary_expr  %prec UNARY
			| SIZEOF '(' typename ')'  %prec HYPERUNARY
			| ALIGNOF unary_expr  %prec UNARY
			| ALIGNOF '(' typename ')'  %prec HYPERUNARY
		
	| .scope new typename %prec '='
			| .scope new '(' nonnull_exprlist ')' typename %prec '='
			| .scope new typespec '(' nonnull_exprlist ')'
			| .scope new typespec '(' typespec ')'
			| .scope new '(' nonnull_exprlist ')' typespec '(' nonnull_exprlist ')'
			| .scope new typespec LEFT_RIGHT
			| .scope new '(' nonnull_exprlist ')' typespec LEFT_RIGHT
			| .scope new typename '=' init %prec '='
			| .scope new '(' nonnull_exprlist ')' typename '=' init %prec '='
		
	/* I am not going to add placement syntax to the below complex rules
	   because Ken says the syntax is illegal. (mrs) */
	/* I'm not sure why this is disallowed.  But since it is, and it
	   doesn't seem difficult to catch it, let's give a message, so
	   the programmer can fix it.  --Ken Raeburn  */
	| .scope new '(' typed_typespecs absdcl ')' '[' nonmomentary_expr ']'
			| .scope new '(' nonempty_type_quals absdcl ')' '[' nonmomentary_expr ']'
		
	| .scope new '(' typed_typespecs absdcl ')'
			| .scope new '(' nonnull_exprlist ')' '(' typed_typespecs absdcl ')'
			| .scope new '(' nonempty_type_quals absdcl ')'
			| .scope new '(' nonnull_exprlist ')' '(' nonempty_type_quals absdcl ')'
			/* Unswallow a ':' which is probably meant for ?: expression.  */
	| .scope new TYPENAME_COLON
			| .scope new '(' nonnull_exprlist ')' TYPENAME_COLON
		
	| delete cast_expr  %prec UNARY
			| delete '[' ']' cast_expr  %prec UNARY
			| delete '[' expr ']' cast_expr %prec UNARY
			;

cast_expr:
	  unary_expr
	| '(' typename ')' expr_no_commas  %prec UNARY
			| '(' typename ')' '{' initlist maybecomma '}'  %prec UNARY
			| HEADOF '(' expr ')'
			| CLASSOF '(' expr ')'
			| CLASSOF '(' TYPENAME ')'
			;

expr_no_commas:
	  cast_expr
	| expr_no_commas '+' expr_no_commas
			| expr_no_commas '-' expr_no_commas
			| expr_no_commas '*' expr_no_commas
			| expr_no_commas '/' expr_no_commas
			| expr_no_commas '%' expr_no_commas
			| expr_no_commas LSHIFT expr_no_commas
			| expr_no_commas RSHIFT expr_no_commas
			| expr_no_commas ARITHCOMPARE expr_no_commas
			| expr_no_commas '<' expr_no_commas
			| expr_no_commas '>' expr_no_commas
			| expr_no_commas EQCOMPARE expr_no_commas
			| expr_no_commas MIN_MAX expr_no_commas
			| expr_no_commas '&' expr_no_commas
			| expr_no_commas '|' expr_no_commas
			| expr_no_commas '^' expr_no_commas
			| expr_no_commas ANDAND expr_no_commas
			| expr_no_commas OROR expr_no_commas
			| expr_no_commas '?' xexpr ':' expr_no_commas
			| expr_no_commas '=' expr_no_commas
			| expr_no_commas ASSIGN expr_no_commas
			| primary DOT_STAR expr_no_commas %prec UNARY
			/* Handle general members.  */
	| object_star expr_no_commas   %prec UNARY
		/* These extensions are not defined.  The second arg to build_m_component_ref
   is old, build_m_component_ref now does an implicit
   build_indirect_ref (x, NULL_PTR) on the second argument.
	| object '&' expr_no_commas   %prec UNARY
			| object unop expr_no_commas  %prec UNARY
			| object '(' typename ')' expr_no_commas  %prec UNARY
			| object primary_no_id  %prec UNARY
		*/
	;

primary:
	IDENTIFIER
			| operator_name
			| CONSTANT
	| string
			| '(' expr ')'
			| '(' error ')'
			| '('
			  compstmt ')'
			| primary '(' nonnull_exprlist ')'
                 template_instantiate_some 	| primary LEFT_RIGHT
                	| primary '[' expr ']'
			| object identifier_or_opname  %prec UNARY
			| object id_scope identifier_or_opname %prec UNARY
			| primary PLUSPLUS
			| primary MINUSMINUS
			/* C++ extensions */
	| THIS
			| TYPE_QUAL '(' nonnull_exprlist ')'
			| typespec '(' nonnull_exprlist ')'
			| typespec LEFT_RIGHT
			/* Stroustrup RTTI */
	| DYNAMIC_CAST '<' typename '>' '(' expr ')'
			| TYPEID '(' expr ')'
			| TYPEID '(' typename ')'
			| SCOPE typespec '(' nonnull_exprlist ')'
			| SCOPE typespec LEFT_RIGHT
			| SCOPE IDENTIFIER
			| SCOPE operator_name
			| id_scope identifier_or_opname  %prec HYPERUNARY
			| id_scope identifier_or_opname '(' nonnull_exprlist ')'
			| id_scope identifier_or_opname LEFT_RIGHT
			| object identifier_or_opname '(' nonnull_exprlist ')'
			| object identifier_or_opname LEFT_RIGHT
			| object id_scope identifier_or_opname '(' nonnull_exprlist ')'
			| object id_scope identifier_or_opname LEFT_RIGHT
			/* p->int::~int() is valid -- 12.4 */
	| object '~' TYPESPEC LEFT_RIGHT
			| object TYPESPEC SCOPE '~' TYPESPEC LEFT_RIGHT
			;

/* Not needed for now.

primary_no_id:
	  '(' expr ')'
			| '(' error ')'
			| '('
			  compstmt ')'
			| primary_no_id '(' nonnull_exprlist ')'
			| primary_no_id LEFT_RIGHT
			| primary_no_id '[' expr ']'
			| primary_no_id PLUSPLUS
			| primary_no_id MINUSMINUS
			| SCOPE IDENTIFIER
			| SCOPE operator_name
			;
*/

new:	  NEW
			| NEW '{' nonnull_exprlist '}'
			;

.scope:
	/* empty  */
			| SCOPE
			;

delete:	  DELETE
			| SCOPE delete
			;

/* Produces a STRING_CST with perhaps more STRING_CSTs chained onto it.  */
string:
	  STRING
	| string STRING
			;

nodecls:
	  /* empty */
			;

object:	  primary '.'
	| primary POINTSAT
			;

object_star: primary POINTSAT_STAR
	;

decl:
	  typed_declspecs initdecls ';'
			/* Normal case: make this fast.  */
	| typed_declspecs declarator ';'
			| declmods notype_initdecls ';'
			/* Normal case: make this fast.  */
	| declmods declarator ';'
			| typed_declspecs ';'
			| declmods ';'
			;

/* Any kind of declarator (thus, all declarators allowed
   after an explicit typespec).  */

declarator:
	  after_type_declarator
	| notype_declarator
	| START_DECLARATOR after_type_declarator
			| START_DECLARATOR notype_declarator
			;

/* Declspecs which contain at least one type specifier or typedef name.
   (Just `const' or `volatile' is not enough.)
   A typedef'd name following these is taken as a name to be declared.  */

typed_declspecs:
	  typespec	%prec HYPERUNARY
			| declmods typespec
			| typespec reserved_declspecs	%prec HYPERUNARY
			| declmods typespec reserved_declspecs
			;

reserved_declspecs:  /* empty
		 */
	  typespecqual_reserved
			| SCSPEC
			| reserved_declspecs typespecqual_reserved
			| reserved_declspecs SCSPEC
			;

/* List of just storage classes and type modifiers.
   A declaration can start with just this, but then it cannot be used
   to redeclare a typedef-name.  */

declmods:
	  TYPE_QUAL
			| SCSPEC
			| declmods TYPE_QUAL
			| declmods SCSPEC
			;


/* Used instead of declspecs where storage classes are not allowed
   (that is, for typenames and structure components).

   C++ can takes storage classes for structure components.
   Don't accept a typedef-name if anything but a modifier precedes it.  */

typed_typespecs:
	  typespec  %prec EMPTY
			| nonempty_type_quals typespec
			| typespec reserved_typespecquals
			| nonempty_type_quals typespec reserved_typespecquals
			;

reserved_typespecquals:
	  typespecqual_reserved
			| reserved_typespecquals typespecqual_reserved
			;

/* A typespec (but not a type qualifier).
   Once we have seen one of these in a declaration,
   if a typedef name appears then it is being redeclared.  */

typespec: structsp
	| TYPESPEC  %prec EMPTY
	| TYPENAME  %prec EMPTY
	| scoped_typename
	| TYPEOF '(' expr ')'
			| TYPEOF '(' typename ')'
			| template_type
	;

/* A typespec that is a reserved word, or a type qualifier.  */

typespecqual_reserved: TYPESPEC
	| TYPE_QUAL
	| structsp
	;

initdecls:
	  initdcl0
	| initdecls ',' initdcl
	;

notype_initdecls:
	  notype_initdcl0
	| notype_initdecls ',' initdcl
	;

maybeasm:
	  /* empty */
			| asm_keyword '(' string ')'
			;

initdcl0:
	  declarator maybe_raises maybeasm maybe_attribute '='
			  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
			| declarator maybe_raises maybeasm maybe_attribute
			;

initdcl:
	  declarator maybe_raises maybeasm maybe_attribute '='
			  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
			| declarator maybe_raises maybeasm maybe_attribute
			;

notype_initdcl0:
	  notype_declarator maybe_raises maybeasm maybe_attribute '='
			  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
			| notype_declarator maybe_raises maybeasm maybe_attribute
			;

/* the * rules are dummies to accept the Apollo extended syntax
   so that the header files compile. */
maybe_attribute:
    /* empty */
	    | ATTRIBUTE '(' '(' attribute_list ')' ')'
            ;

attribute_list
    : attrib
	    | attribute_list ',' attrib
	    ;

attrib
    : IDENTIFIER
	    | IDENTIFIER '(' CONSTANT ')'
	    | IDENTIFIER '(' IDENTIFIER ',' CONSTANT ',' CONSTANT ')'
	    ;

/* A nonempty list of identifiers, including typenames.  */
identifiers_or_typenames:
	identifier
			| identifiers_or_typenames ',' identifier
			;

init:
	  expr_no_commas %prec '='
	| '{' '}'
			| '{' initlist '}'
			| '{' initlist ',' '}'
			| error
			;

/* This chain is built in reverse order,
   and put in forward order where initlist is used.  */
initlist:
	  init
			| initlist ',' init
			/* These are for labeled elements.  */
	| '[' expr_no_commas ']' init
			| initlist ',' CASE expr_no_commas ':' init
			| identifier ':' init
			| initlist ',' identifier ':' init
			;

structsp:
	  ENUM identifier '{'
			  enumlist maybecomma_warn '}'
			| ENUM identifier '{' '}'
			| ENUM '{'
			  enumlist maybecomma_warn '}'
			| ENUM '{' '}'
			| ENUM identifier
		
	/* C++ extensions, merged with C to avoid shift/reduce conflicts */
	| class_head left_curly opt.component_decl_list '}'
			| class_head  %prec EMPTY
			;

maybecomma:
	  /* empty */
	| ','
	;

maybecomma_warn:
	  /* empty */
	| ','
			;

aggr:	  AGGR
	| aggr SCSPEC
			| aggr TYPESPEC
			| aggr TYPE_QUAL
			| aggr AGGR
			;

named_class_head_sans_basetype:
	  aggr identifier
			| aggr template_type_name  %prec EMPTY
			| aggr TYPENAME_COLON
			| aggr template_type_name '{'
		'{'	| aggr template_type_name ':'
			;

named_class_head_sans_basetype_defn:
	  aggr identifier_defn
			;

named_class_head:
	  named_class_head_sans_basetype
			  maybe_base_class_list %prec EMPTY
			|
	  named_class_head_sans_basetype_defn
			  maybe_base_class_list %prec EMPTY
			;

unnamed_class_head: aggr '{'
		'{'	;

class_head: unnamed_class_head | named_class_head ;

maybe_base_class_list:
	  /* empty */
			| ':'  %prec EMPTY
			| ':' base_class_list  %prec EMPTY
			;

base_class_list:
	  base_class
	| base_class_list ',' base_class
			;

base_class:
	  base_class.1
			| scoped_base_class
			| base_class_visibility_list base_class.1
			| base_class_visibility_list scoped_base_class
			;

scoped_base_class:
	  base_class.1 SCOPED_TYPENAME
			;
base_class.1:
	  template_type_name tmpl.2 template_instantiation
			| identifier
	;

base_class_visibility_list:
	  VISSPEC
	| SCSPEC
			| base_class_visibility_list VISSPEC
			| base_class_visibility_list SCSPEC
			;

left_curly: '{'
			;

opt.component_decl_list:
	/* empty */
			| component_decl_list
			| opt.component_decl_list VISSPEC ':' component_decl_list
			| opt.component_decl_list VISSPEC ':'
	;

/* Note: we no longer warn about the semicolon after a component_decl_list.
   ARM $9.2 says that the semicolon is optional, and therefore allowed.  */
component_decl_list:
	  component_decl
			| component_decl_list component_decl
			| component_decl_list ';'
	;

component_decl:
	  typed_declspecs components ';'
			| typed_declspecs '(' parmlist ')' ';'
			| typed_declspecs '(' parmlist ')' '}'
		'}'	| typed_declspecs LEFT_RIGHT ';'
			| typed_declspecs LEFT_RIGHT '}'
		'}'	| declmods components ';'
			/* Normal case: make this fast.  */
	| declmods declarator ';'
			| declmods components '}'
		'}'	| declmods '(' parmlist ')' ';'
			| declmods '(' parmlist ')' '}'
		'}'	| declmods LEFT_RIGHT ';'
			| declmods LEFT_RIGHT '}'
		'}'	| ':' expr_no_commas ';'
			| ':' expr_no_commas '}'
		'}'	| error
		
	/* C++: handle constructors, destructors and inline functions */
	/* note that INLINE is like a TYPESPEC */
	| fn.def2 ':' /* base_init compstmt */
			| fn.def2 '{' /* nodecls compstmt */
			| notype_declarator maybe_raises ';'
			| notype_declarator maybe_raises '}'
		'}'	;

components:
	  /* empty: possibly anonymous */
			| component_declarator0
	| components ',' component_declarator
			;

component_declarator0:
	  declarator maybe_raises maybeasm maybe_attribute
			| declarator maybe_raises maybeasm maybe_attribute '=' init
			| IDENTIFIER ':' expr_no_commas maybe_attribute
			| TYPENAME ':' expr_no_commas maybe_attribute
			| ':' expr_no_commas maybe_attribute
			;

component_declarator:
	  declarator maybe_raises maybeasm maybe_attribute
			| declarator maybe_raises maybeasm maybe_attribute '=' init
			| IDENTIFIER ':' expr_no_commas maybe_attribute
			| TYPENAME ':' expr_no_commas maybe_attribute
			| ':' expr_no_commas maybe_attribute
			;

/* We chain the enumerators in reverse order.
   Because of the way enums are built, the order is
   insignificant.  Take advantage of this fact.  */

enumlist:
	  enumerator
	| enumlist ',' enumerator
			;

enumerator:
	  identifier
			| identifier '=' expr_no_commas
			;

/* ANSI type-id (8.1) */
typename:
	  typed_typespecs absdcl
			| nonempty_type_quals absdcl
			;

/* ANSI abstract-declarator (8.1) */
absdcl:   /* an abstract declarator */
	/* empty */ %prec EMPTY
			| absdcl1  %prec EMPTY
	| START_DECLARATOR absdcl1  %prec EMPTY
			;

nonempty_type_quals:
	  TYPE_QUAL
			| nonempty_type_quals TYPE_QUAL
			;

type_quals:
	  /* empty */
			| type_quals TYPE_QUAL
			;

/* These rules must follow the rules for function declarations
   and component declarations.  That way, longer rules are preferred.  */

/* An expression which will not live on the momentary obstack.  */
nonmomentary_expr:
	 expr
	 ;	
/* A declarator that is allowed only after an explicit typespec.  */
/* may all be followed by prec '.' */
after_type_declarator:
	  after_type_declarator '(' nonnull_exprlist ')' type_quals  %prec '.'
			| after_type_declarator '(' parmlist ')' type_quals  %prec '.'
			| after_type_declarator LEFT_RIGHT type_quals  %prec '.'
			| after_type_declarator '(' error ')' type_quals  %prec '.'
			| after_type_declarator '[' nonmomentary_expr ']'
			| after_type_declarator '[' ']'
			| '(' after_type_declarator_no_typename ')'
			| '(' '*' type_quals after_type_declarator ')'
			| PAREN_STAR_PAREN
			| after_type_member_declarator
	| '(' '&' type_quals after_type_declarator ')'
			| '*' type_quals after_type_declarator  %prec UNARY
			| '&' type_quals after_type_declarator  %prec UNARY
			| TYPENAME
	;

after_type_declarator_no_typename:
	  after_type_declarator_no_typename '(' nonnull_exprlist ')' type_quals  %prec '.'
			| after_type_declarator_no_typename '(' parmlist ')' type_quals  %prec '.'
			| after_type_declarator_no_typename LEFT_RIGHT type_quals  %prec '.'
			| after_type_declarator_no_typename '(' error ')' type_quals  %prec '.'
			| after_type_declarator_no_typename '[' nonmomentary_expr ']'
			| after_type_declarator_no_typename '[' ']'
			| '(' after_type_declarator_no_typename ')'
			| PAREN_STAR_PAREN
			| after_type_member_declarator
	| '*' type_quals after_type_declarator  %prec UNARY
			| '&' type_quals after_type_declarator  %prec UNARY
			;

/* A declarator allowed whether or not there has been
   an explicit typespec.  These cannot redeclare a typedef-name.  */

notype_declarator:
	  notype_declarator '(' nonnull_exprlist ')' type_quals  %prec '.'
			| notype_declarator '(' parmlist ')' type_quals  %prec '.'
			| notype_declarator LEFT_RIGHT type_quals  %prec '.'
			| notype_declarator '(' error ')' type_quals  %prec '.'
			| '(' notype_declarator ')'
			| '*' type_quals notype_declarator  %prec UNARY
			| '&' type_quals notype_declarator  %prec UNARY
			| notype_declarator '[' nonmomentary_expr ']'
			| notype_declarator '[' ']'
			| IDENTIFIER
		
	/* C++ extensions.  */
	| operator_name
		
	| '~' TYPENAME
			| '~' IDENTIFIER
		        | '~' PTYPENAME
                	| id_scope see_typename notype_declarator  %prec '('
			| id_scope see_typename TYPENAME  %prec '('
			| id_scope see_typename TYPENAME '(' nonnull_exprlist ')' type_quals  %prec '.'
			| id_scope see_typename TYPENAME '(' parmlist ')' type_quals  %prec '.'
			| id_scope see_typename TYPENAME LEFT_RIGHT type_quals  %prec '.'
			| id_scope see_typename TYPENAME '(' error ')' type_quals  %prec '.'
			/* For constructor templates.  */
	| id_scope see_typename PTYPENAME  %prec '('
			| id_scope see_typename PTYPENAME '(' nonnull_exprlist ')' type_quals  %prec '.'
			| id_scope see_typename PTYPENAME '(' parmlist ')' type_quals  %prec '.'
			| id_scope see_typename PTYPENAME LEFT_RIGHT type_quals  %prec '.'
			| id_scope see_typename PTYPENAME '(' error ')' type_quals  %prec '.'
			| SCOPE see_typename notype_declarator
			;

id_scope:	typename_scope
			| IDENTIFIER SCOPE
			| template_type SCOPE /* try_for_typename %prec EMPTY */
			;

typename_scope:
	TYPENAME SCOPE;

scoped_typename: SCOPED_TYPENAME
	| template_type SCOPED_TYPENAME
		/*	| template_type SCOPE try_for_typename TYPENAME
		 */
	;

absdcl1:  /* a nonempty abstract declarator */
	  '(' absdcl1 ')'
			  /* `(typedef)1' is `int'.  */
	| '*' type_quals absdcl1  %prec EMPTY
			| '*' type_quals  %prec EMPTY
			| PAREN_STAR_PAREN
			| '(' abs_member_declarator ')'
			| '&' type_quals absdcl1 %prec EMPTY
			| '&' type_quals %prec EMPTY
			| absdcl1 '(' parmlist ')' type_quals  %prec '.'
			| absdcl1 LEFT_RIGHT type_quals  %prec '.'
			| absdcl1 '[' nonmomentary_expr ']'  %prec '.'
			| absdcl1 '[' ']'  %prec '.'
			| '(' parmlist ')' type_quals  %prec '.'
			| LEFT_RIGHT type_quals  %prec '.'
			| '[' nonmomentary_expr ']'  %prec '.'
			| '[' ']'  %prec '.'
			;

abs_member_declarator:
	  id_scope see_typename '*' type_quals
			| id_scope see_typename '*' type_quals absdcl1
			| id_scope see_typename '&' type_quals
			| id_scope see_typename '&' type_quals absdcl1
			;

after_type_member_declarator:
	  id_scope see_typename '*' type_quals after_type_declarator
			| id_scope see_typename '&' type_quals after_type_declarator
			;

/* For C++, decls and stmts can be intermixed, so we don't need to
   have a special rule that won't start parsing the stmt section
   until we have a stmt that parses without errors.  */

stmts:
	  stmt
	| errstmt
	| stmts stmt
	| stmts errstmt
	;

errstmt:  error ';'
	;

/* build the LET_STMT node before parsing its contents,
  so that any LET_STMTs within the context can have their display pointers
  set up to point at this one.  */

.pushlevel:  /* empty */
			;

/* Read zero or more forward-declarations for labels
   that nested functions can jump to.  */
maybe_label_decls:
	  /* empty */
	| label_decls
			;

label_decls:
	  label_decl
	| label_decls label_decl
	;

label_decl:
	  LABEL identifiers_or_typenames ';'
			;

/* This is the body of a function definition.
   It causes syntax errors to ignore to the next openbrace.  */
compstmt_or_error:
	  compstmt
			| error compstmt
	;

compstmt: '{' .pushlevel '}'
			| '{' .pushlevel maybe_label_decls stmts '}'
			| '{' .pushlevel maybe_label_decls error '}'
			;

simple_if:
	  IF
			  .pushlevel paren_cond_or_null
			  partially_scoped_stmt
	;

implicitly_scoped_stmt:
	  compstmt
			| .pushlevel simple_stmt
			;

stmt:
	  compstmt
			| simple_stmt
	;

simple_stmt:
	  decl
			| expr ';'
			| simple_if ELSE
			  partially_scoped_stmt
			| simple_if %prec IF
			| WHILE
			  .pushlevel paren_cond_or_null
			  already_scoped_stmt
			| DO
			  implicitly_scoped_stmt WHILE
			  paren_expr_or_null ';'
			| forhead.1
			  .pushlevel xcond ';'
			  xexpr ')'
		/* Don't let the tree nodes for $7 be discarded
		   by clear_momentary during the parsing of the next stmt.  */
			  already_scoped_stmt
			| forhead.2
			  .pushlevel xcond ';'
			  xexpr ')'
		/* Don't let the tree nodes for $7 be discarded
		   by clear_momentary during the parsing of the next stmt.  */
			  already_scoped_stmt
			| SWITCH .pushlevel '(' condition ')'
			  partially_scoped_stmt
			| CASE expr_no_commas ':'
			  stmt
	| CASE expr_no_commas RANGE expr_no_commas ':'
			  stmt
	| DEFAULT ':'
			  stmt
	| BREAK ';'
			| CONTINUE ';'
			| RETURN ';'
			| RETURN expr ';'
			| asm_keyword maybe_type_qual '(' string ')' ';'
			/* This is the case with just output operands.  */
	| asm_keyword maybe_type_qual '(' string ':' asm_operands ')' ';'
			/* This is the case with input operands as well.  */
	| asm_keyword maybe_type_qual '(' string ':' asm_operands ':' asm_operands ')' ';'
			/* This is the case with clobbered registers as well.  */
	| asm_keyword maybe_type_qual '(' string ':' asm_operands ':'
  	  asm_operands ':' asm_clobbers ')' ';'
			| GOTO '*' expr ';'
			| GOTO identifier ';'
			| label_colon stmt
			| label_colon '}'
		'}'	| ';'
		
	/* Exception handling extensions.  */
	| ANSI_THROW ';' 	| ANSI_THROW expr ';' 	| THROW raise_identifier '(' nonnull_exprlist ')' ';'
			| THROW raise_identifier LEFT_RIGHT ';'
			| RAISE raise_identifier '(' nonnull_exprlist ')' ';'
			| RAISE raise_identifier LEFT_RIGHT ';'
			| RAISE identifier ';'
			| try EXCEPT identifier '{'
			  except_stmts '}'
			| try error
			| ansi_try ansi_dummy ansi_dummy
			  ansi_except_stmts
			| try RERAISE raise_identifiers /* ';' checked for at bottom.  */
			| try  %prec EMPTY
			;

try:	  try_head '}'
		/* An empty try block is degenerate, but it's better to
		   do extra work here than to do all the special-case work
		   everywhere else.  */
			| try_head stmts '}'
			| try_head error '}'
			;

label_colon:
	  IDENTIFIER ':'
			| PTYPENAME ':'
			| TYPENAME_COLON
			;

try_head: TRY '{'  .pushlevel
;
ansi_try:	  ansi_try_head '}'
		/* An empty try block is degenerate, but it's better to
		   do extra work here than to do all the special-case work
		   everywhere else.  */
			| ansi_try_head stmts '}'
			| ansi_try_head error '}'
			;

ansi_dummy: ; /* Temporary place-holder. */
ansi_try_head: ANSI_TRY '{'  .pushlevel
;
except_stmts:
	  /* empty */
			| except_stmts raise_identifier
			  compstmt
			| except_stmts DEFAULT
			  compstmt
			;

optional_identifier:
	  /* empty */
			| identifier ;

ansi_except_stmts:
	  /* empty */
			| ansi_except_stmts CATCH '(' typename optional_identifier ')'
			  compstmt
			;

forhead.1:
	  FOR '(' ';'
			| FOR '(' expr ';'
			| FOR '(' '{' '}'
			;

forhead.2:
	  FOR '(' decl
			| FOR '(' error ';'
			| FOR '(' '{' .pushlevel stmts '}'
			| FOR '(' '{' .pushlevel error '}'
			;

/* Either a type-qualifier or nothing.  First thing in an `asm' statement.  */

maybe_type_qual:
	/* empty */
			| TYPE_QUAL
			;

xexpr:
	/* empty */
			| expr
	| error
			;

/* These are the operands other than the first string and colon
   in  asm ("addextend %2,%1": "=dm" (x), "0" (y), "g" (*x))  */
asm_operands: /* empty */
			| nonnull_asm_operands
	;

nonnull_asm_operands:
	  asm_operand
	| nonnull_asm_operands ',' asm_operand
			;

asm_operand:
	  STRING '(' expr ')'
			;

asm_clobbers:
	  STRING
			| asm_clobbers ',' STRING
			;

/* This is what appears inside the parens in a function declarator.
   Its value is represented in the format that grokdeclarator expects.

   In C++, declaring a function with no parameters
   means that that function takes *no* parameters.  */
parmlist:  /* empty */
			| parms
  			| parms ',' ELLIPSIS
			/* C++ allows an ellipsis without a separating ',' */
	| parms ELLIPSIS
			| ELLIPSIS
			| TYPENAME_ELLIPSIS
			| parms TYPENAME_ELLIPSIS
			| parms ':'
			;

/* A nonempty list of parameter declarations or type names.  */
parms:
	  parm
			| parm '=' init
			| parms ',' parm
			| parms ',' parm '=' init
			| parms ',' bad_parm
			| parms ',' bad_parm '=' init
			;

/* A single parameter declaration or parameter type name,
   as found in a parmlist.  The first four cases make up for 10%
   of the time spent parsing C++.  We cannot use them because
   of `int id[]' which won't get parsed properly.  */
parm:
/*
	  typed_declspecs dont_see_typename '*' IDENTIFIER
			| typed_declspecs dont_see_typename '&' IDENTIFIER
			| TYPENAME IDENTIFIER
			| TYPESPEC IDENTIFIER
			| */
	  typed_declspecs dont_see_typename abs_or_notype_decl
			| declmods dont_see_typename abs_or_notype_decl
			;

abs_or_notype_decl: absdcl
	| notype_declarator
	| START_DECLARATOR notype_declarator
			;

see_typename: type_quals
		;

dont_see_typename: /* empty */
		;

/*
try_for_typename:
        	;
*/

bad_parm:
	  abs_or_notype_decl
			;

maybe_raises:
	  /* empty */
			| RAISES raise_identifiers  %prec EMPTY
			| ANSI_THROW '(' ansi_raise_identifiers  ')' %prec EMPTY
			;

raise_identifier:
	  ALL
			| IDENTIFIER
			| TYPENAME
			| SCOPE IDENTIFIER
			| SCOPE TYPENAME
			| id_scope IDENTIFIER
			| scoped_typename
	;

ansi_raise_identifier:
	  typename
			;

raise_identifiers:
	  raise_identifier
	| raise_identifiers ',' raise_identifier
			;

ansi_raise_identifiers:
	  ansi_raise_identifier
	| ansi_raise_identifiers ',' ansi_raise_identifier
			;

operator_name:
	  OPERATOR '*'
			| OPERATOR '/'
			| OPERATOR '%'
			| OPERATOR '+'
			| OPERATOR '-'
			| OPERATOR '&'
			| OPERATOR '|'
			| OPERATOR '^'
			| OPERATOR '~'
			| OPERATOR ','
			| OPERATOR ARITHCOMPARE
			| OPERATOR '<'
			| OPERATOR '>'
			| OPERATOR EQCOMPARE
			| OPERATOR ASSIGN
			| OPERATOR '='
			| OPERATOR LSHIFT
			| OPERATOR RSHIFT
			| OPERATOR PLUSPLUS
			| OPERATOR MINUSMINUS
			| OPERATOR ANDAND
			| OPERATOR OROR
			| OPERATOR '!'
			| OPERATOR '?' ':'
			| OPERATOR MIN_MAX
			| OPERATOR POINTSAT  %prec EMPTY
			| OPERATOR POINTSAT_STAR  %prec EMPTY
			| OPERATOR LEFT_RIGHT
			| OPERATOR '[' ']'
			| OPERATOR NEW
		/*
	| OPERATOR NEW '[' ']'
		*/
	| OPERATOR DELETE
		/*
	| OPERATOR DELETE '[' ']'
		*/

	/* These should do `groktypename' and set up TREE_HAS_X_CONVERSION
	   here, rather than doing it in class.c .  */
	| OPERATOR typed_typespecs absdcl
			| OPERATOR error
			;

%%
