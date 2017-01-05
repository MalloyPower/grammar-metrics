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
%token HEADOF CLASSOF SIGOF
%token ATTRIBUTE EXTENSION LABEL

/* the reserved words... C++ extensions */
%token <ttype> AGGR
%token <itype> VISSPEC
%token DELETE NEW OVERLOAD THIS OPERATOR CXX_TRUE CXX_FALSE
%token LEFT_RIGHT TEMPLATE
%token TYPEID DYNAMIC_CAST STATIC_CAST REINTERPRET_CAST CONST_CAST
%token <itype> SCOPE

/* Define the operator tokens and their precedences.
   The value is an integer because, if used, it is the tree code
   to use in the expression made from the operator.  */

%left EMPTY			/* used to resolve s/r with epsilon */

%left error

/* Add precedence rules to solve dangling else s/r conflict */
%nonassoc IF
%nonassoc ELSE

%left IDENTIFIER TYPENAME PTYPENAME SCSPEC TYPESPEC TYPE_QUAL ENUM AGGR ELLIPSIS TYPEOF SIGOF OPERATOR

%left '{' ',' ';'

%right <code> ASSIGN '='
%right <code> '?' ':'
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
%left <code> POINTSAT_STAR DOT_STAR
%right <code> UNARY PLUSPLUS MINUSMINUS '~'
%left HYPERUNARY
%left <ttype> PAREN_STAR_PAREN LEFT_RIGHT
%left <code> POINTSAT '.' '(' '['

%right SCOPE			/* C++ extension */
%nonassoc NEW DELETE TRY CATCH THROW

%type <code> unop

%type <ttype> identifier IDENTIFIER TYPENAME CONSTANT expr nonnull_exprlist
%type <ttype> paren_expr_or_null nontrivial_exprlist
%type <ttype> expr_no_commas cast_expr unary_expr primary string STRING
%type <ttype> typed_declspecs reserved_declspecs boolean.literal
%type <ttype> typed_typespecs reserved_typespecquals
%type <ttype> declmods typespec typespecqual_reserved
%type <ttype> SCSPEC TYPESPEC TYPE_QUAL nonempty_type_quals maybe_type_qual
%type <itype> initdecls notype_initdecls initdcl	/* C++ modification */
%type <ttype> init initlist maybeasm
%type <ttype> asm_operands nonnull_asm_operands asm_operand asm_clobbers
%type <ttype> maybe_attribute attributes attribute attribute_list attrib
%type <ttype> any_word

%type <ttype> compstmt implicitly_scoped_stmt

%type <ttype> declarator notype_declarator after_type_declarator
%type <ttype> direct_notype_declarator direct_after_type_declarator

%type <ttype> structsp opt.component_decl_list component_decl_list
%type <ttype> component_decl components component_declarator
%type <ttype> notype_components notype_component_declarator
%type <ttype> after_type_component_declarator after_type_component_declarator0
%type <ttype> notype_component_declarator0 component_decl_1
%type <ttype> enumlist enumerator
%type <ttype> type_id absdcl type_quals
%type <ttype> direct_abstract_declarator conversion_declarator
%type <ttype> new_type_id new_declarator direct_new_declarator
%type <ttype> xexpr parmlist parms parm bad_parm full_parm
%type <ttype> identifiers_or_typenames
%type <ttype> fcast_or_absdcl regcast_or_absdcl sub_cast_expr
%type <ttype> expr_or_declarator complex_notype_declarator
%type <ttype> notype_unqualified_id unqualified_id qualified_id
%type <ttype> overqualified_id notype_qualified_id
%type <ttype> complex_direct_notype_declarator functional_cast
%type <ttype> named_parm complex_parmlist typed_declspecs1 parms_comma

/* C++ extensions */
%token <ttype> TYPENAME_ELLIPSIS PTYPENAME
%token <ttype> PRE_PARSED_FUNCTION_DECL EXTERN_LANG_STRING ALL
%token <ttype> PRE_PARSED_CLASS_DECL
%type <ttype> fn.def1 /* Not really! */
%type <ttype> fn.def2 return_id
%type <ttype> named_class_head named_class_head_sans_basetype
%type <ttype> unnamed_class_head
%type <ttype> class_head base_class_list
%type <itype> base_class_access_list
%type <ttype> base_class maybe_base_class_list base_class.1
%type <ttype> maybe_raises ansi_raise_identifier ansi_raise_identifiers
%type <ttype> component_declarator0
%type <ttype> forhead.1 operator_name
%type <ttype> object aggr
%type <itype> new delete
/* %type <ttype> primary_no_id */
%type <ttype> nonmomentary_expr
%type <itype> forhead.2 initdcl0 notype_initdcl0 member_init_list
%type <ttype> template_header template_parm_list template_parm
%type <ttype> template_type_parm
%type <ttype> template_type template_arg_list template_arg
%type <ttype> template_instantiation template_type_name tmpl.2
%type <ttype> template_instantiate_once template_instantiate_some
%type <itype> fn_tmpl_end
/* %type <itype> try_for_typename */
%type <ttype> condition xcond paren_cond_or_null
%type <ttype> type_name nested_name_specifier nested_type ptr_to_mem
%type <ttype> qualified_type_name complete_type_name notype_identifier
%type <ttype> complex_type_name nested_name_specifier_1
%type <itype> nomods_initdecls nomods_initdcl0
%type <ttype> new_initializer new_placement specialization type_specifier_seq

/* in order to recognize aggr tags as defining and thus shadowing. */
%token TYPENAME_DEFN IDENTIFIER_DEFN PTYPENAME_DEFN
%type <ttype> named_class_head_sans_basetype_defn 
%type <ttype> identifier_defn IDENTIFIER_DEFN TYPENAME_DEFN PTYPENAME_DEFN

%type <strtype> .pushlevel

/* spew.c depends on this being the last token.  Define
   any new tokens before this one!  */
%token END_OF_SAVED_INPUT


%%
program: /* empty */
	| extdefs
			;

/* the reason for the strange actions in this rule
 is so that notype_initdecls when reached via datadef
 can find a valid list of type and sc specs in $0. */

extdefs:
	   lang_extdef
			| extdefs lang_extdef
			;

.hush_warning:
			;
.warning_ok:
			;

asm_keyword:
	  ASM_KEYWORD
	| GCC_ASM_KEYWORD
	;

lang_extdef:
	  	  extdef
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

template_type_parm:
	  aggr
			| aggr identifier
			;

template_parm:
	/* The following rules introduce a new reduce/reduce
	   conflict on the ',' and '>' input tokens: they are valid
	   prefixes for a `structsp', which means they could match a
	   nameless parameter.  See 14.6, paragraph 3.
	   By putting them before the `parm' rule, we get
	   their match before considering them nameless parameter
	   declarations.  */
	  template_type_parm
			| template_type_parm '=' typespec
			| full_parm
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
			| template_header declmods notype_declarator fn_tmpl_end
			/* Try to recover from syntax errors in templates.  */
	| template_header error '}'		| template_header error ';'		;

fn_tmpl_end: '{'		'{'	| ':'				| ';'				| '='				| RETURN			;

datadef:
	  nomods_initdecls ';'
			| declmods notype_initdecls ';'
			/* Normal case to make fast: "const i;".  */
	| declmods notype_declarator ';'
			| typed_declspecs initdecls ';'
			/* Normal case: make this fast.  */
	| typed_declspecs declarator ';'
		        | declmods ';'
	  	| explicit_instantiation ';'
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
			| PRE_PARSED_FUNCTION_DECL
			;

/* more C++ complexity.  See component_decl for a comment on the
   reduce/reduce conflict introduced by these rules.  */
fn.def2:
	  typed_declspecs '(' parmlist ')' type_quals maybe_raises
			| typed_declspecs LEFT_RIGHT type_quals maybe_raises
			| typed_declspecs declarator maybe_raises
			| declmods notype_declarator maybe_raises
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
			| notype_identifier '(' nonnull_exprlist ')'
			| notype_identifier LEFT_RIGHT
			| complete_type_name '(' nonnull_exprlist ')'
			| complete_type_name LEFT_RIGHT
			/* GNU extension */
	| notype_qualified_id '(' nonnull_exprlist ')'
			| notype_qualified_id LEFT_RIGHT
			;

identifier:
	  IDENTIFIER
	| TYPENAME
	| PTYPENAME
	;

notype_identifier:
	  IDENTIFIER
	| PTYPENAME %prec EMPTY
	;

identifier_defn:
	  IDENTIFIER_DEFN
	| TYPENAME_DEFN
	| PTYPENAME_DEFN
	;

explicit_instantiation:
	  TEMPLATE specialization template_instantiation
			| TEMPLATE typed_declspecs declarator
			| SCSPEC TEMPLATE specialization template_instantiation
			| SCSPEC TEMPLATE typed_declspecs declarator
			;

template_type:
	  template_type_name tmpl.2 template_instantiation
			;

template_type_name:
	  PTYPENAME '<' template_arg_list '>'
			| PTYPENAME '<' '>'
			| TYPENAME  '<' template_arg_list '>'
			;

tmpl.2: 
	  /* empty */ %prec EMPTY
			;

template_arg_list:
	  template_arg
			| template_arg_list ',' template_arg
			;

template_arg:
	  type_id
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

expr:	  nontrivial_exprlist
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
	type_specifier_seq declarator maybe_raises maybeasm maybe_attribute '='
			init
			| expr
	;

already_scoped_stmt:
	  '{' '}'
			| '{' maybe_label_decls stmts '}'
			| '{' maybe_label_decls error '}'
			| simple_stmt
	;


nontrivial_exprlist:
	  expr_no_commas ',' expr_no_commas
			| expr_no_commas ',' error
			| nontrivial_exprlist ',' expr_no_commas
			| nontrivial_exprlist ',' error
			;

nonnull_exprlist:
	  expr_no_commas
			| nontrivial_exprlist
	;

unary_expr:
	  primary %prec UNARY
			/* __extension__ turns off -pedantic for following primary.  */
	| EXTENSION
			  cast_expr	  %prec UNARY
			| '*' cast_expr   %prec UNARY
			| '&' cast_expr   %prec UNARY
			| '~' cast_expr
			| unop cast_expr  %prec UNARY
			/* Refer to the address of a label as a pointer.  */
	| ANDAND identifier
			| SIZEOF unary_expr  %prec UNARY
			| SIZEOF '(' type_id ')'  %prec HYPERUNARY
			| ALIGNOF unary_expr  %prec UNARY
			| ALIGNOF '(' type_id ')'  %prec HYPERUNARY
		
	/* The %prec EMPTY's here are required by the = init initializer
	   syntax extension; see below.  */
	| new new_type_id %prec EMPTY
			| new new_type_id new_initializer
			| new new_placement new_type_id %prec EMPTY
			| new new_placement new_type_id new_initializer
			| new '(' type_id ')' %prec EMPTY
			| new '(' type_id ')' new_initializer
			| new new_placement '(' type_id ')' %prec EMPTY
			| new new_placement '(' type_id ')' new_initializer
		
	| delete cast_expr  %prec UNARY
			| delete '[' ']' cast_expr  %prec UNARY
			| delete '[' expr ']' cast_expr %prec UNARY
			;

new_placement:
	  '(' nonnull_exprlist ')'
			| '{' nonnull_exprlist '}'
			;

new_initializer:
	  '(' nonnull_exprlist ')'
			| LEFT_RIGHT
			| '(' typespec ')'
			/* GNU extension so people can use initializer lists.  Note that
	   this alters the meaning of `new int = 1', which was previously
	   syntactically valid but semantically invalid.  */
	| '=' init
			;

/* This is necessary to postpone reduction of `int ((int)(int)(int))'.  */
regcast_or_absdcl:
	  '(' type_id ')' %prec EMPTY
			| regcast_or_absdcl '(' type_id ')' %prec EMPTY
			;

cast_expr:
	  sub_cast_expr
	| regcast_or_absdcl sub_cast_expr  %prec UNARY
			| regcast_or_absdcl '{' initlist maybecomma '}'  %prec UNARY
			;

sub_cast_expr:
	  unary_expr
	| HEADOF '(' expr ')'
			| CLASSOF '(' expr ')'
			| CLASSOF '(' TYPENAME ')'
			;

expr_no_commas:
	  cast_expr
	/* Handle general members.  */
	| expr_no_commas POINTSAT_STAR expr_no_commas
			| expr_no_commas DOT_STAR expr_no_commas
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
			| THROW
			| THROW expr_no_commas
		/* These extensions are not defined.  The second arg to build_m_component_ref
   is old, build_m_component_ref now does an implicit
   build_indirect_ref (x, NULL_PTR) on the second argument.
	| object '&' expr_no_commas   %prec UNARY
			| object unop expr_no_commas  %prec UNARY
			| object '(' type_id ')' expr_no_commas  %prec UNARY
			| object primary_no_id  %prec UNARY
		*/
	;

notype_unqualified_id:
	  '~' see_typename identifier
			| operator_name
	| IDENTIFIER
	| PTYPENAME %prec EMPTY
	;

unqualified_id:
	  notype_unqualified_id
	| TYPENAME
	;

expr_or_declarator:
	  notype_unqualified_id
	| notype_qualified_id
	| '*' expr_or_declarator %prec UNARY
			| '&' expr_or_declarator %prec UNARY
			;

direct_notype_declarator:
	  complex_direct_notype_declarator
	| notype_unqualified_id
	| notype_qualified_id
			;

primary:
	  notype_unqualified_id
				
	| CONSTANT
	| boolean.literal
	| string
			| '(' expr ')'
			| '(' error ')'
			| '('
			  compstmt ')'
			| primary '(' nonnull_exprlist ')'
                 template_instantiate_some 	| primary LEFT_RIGHT
                	| primary '[' expr ']'
			| primary PLUSPLUS
			| primary MINUSMINUS
			/* C++ extensions */
	| THIS
			| TYPE_QUAL '(' nonnull_exprlist ')'
			| functional_cast
	| DYNAMIC_CAST '<' type_id '>' '(' expr ')'
			| STATIC_CAST '<' type_id '>' '(' expr ')'
			| REINTERPRET_CAST '<' type_id '>' '(' expr ')'
			| CONST_CAST '<' type_id '>' '(' expr ')'
			| TYPEID '(' expr ')'
			| TYPEID '(' type_id ')'
			| global_scope IDENTIFIER
			| global_scope operator_name
			| overqualified_id %prec HYPERUNARY
			| overqualified_id '(' nonnull_exprlist ')'
			| overqualified_id LEFT_RIGHT
			| object unqualified_id  %prec UNARY
			| object qualified_id %prec UNARY
			| object unqualified_id '(' nonnull_exprlist ')'
			| object unqualified_id LEFT_RIGHT
			| object qualified_id '(' nonnull_exprlist ')'
			| object qualified_id LEFT_RIGHT
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
			| global_scope NEW
			;

delete:	  DELETE
			| global_scope delete
			;

boolean.literal:
	  CXX_TRUE
			| CXX_FALSE
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

decl:
	/* Normal case: make this fast.  */
	  typespec declarator ';'
			| typed_declspecs declarator ';'
			| typespec initdecls ';'
			| typed_declspecs initdecls ';'
			| declmods notype_initdecls ';'
			| typed_declspecs ';'
			| declmods ';'
			;

/* Any kind of declarator (thus, all declarators allowed
   after an explicit typespec).  */

declarator:
	  after_type_declarator %prec EMPTY
	| notype_declarator %prec EMPTY
	;

/* This is necessary to postpone reduction of `int()()()()'.  */
fcast_or_absdcl:
	  LEFT_RIGHT %prec EMPTY
			| fcast_or_absdcl LEFT_RIGHT %prec EMPTY
			;

/* ANSI type-id (8.1) */
type_id:
	  typed_typespecs absdcl
			| nonempty_type_quals absdcl
			| typespec absdcl
			| typed_typespecs %prec EMPTY
			| nonempty_type_quals %prec EMPTY
			;

/* Declspecs which contain at least one type specifier or typedef name.
   (Just `const' or `volatile' is not enough.)
   A typedef'd name following these is taken as a name to be declared.  */

typed_declspecs:
	  typed_typespecs %prec EMPTY
	| typed_declspecs1
	  ;
typed_declspecs1:
	  declmods typespec
			| typespec reserved_declspecs	%prec HYPERUNARY
			| declmods typespec reserved_declspecs
			| declmods typespec reserved_typespecquals
			| declmods typespec reserved_typespecquals reserved_declspecs
			;

reserved_declspecs:
	  SCSPEC
			| reserved_declspecs typespecqual_reserved
			| reserved_declspecs SCSPEC
			;

/* List of just storage classes and type modifiers.
   A declaration can start with just this, but then it cannot be used
   to redeclare a typedef-name.  */

declmods:
	  nonempty_type_quals %prec EMPTY
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
	| complete_type_name
	| TYPEOF '(' expr ')'
			| TYPEOF '(' type_id ')'
			| SIGOF '(' expr ')'
			| SIGOF '(' type_id ')'
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

nomods_initdecls:
	  nomods_initdcl0
	| nomods_initdecls ',' initdcl
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

nomods_initdcl0:
	  notype_declarator maybe_raises maybeasm maybe_attribute '='
			  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
			| notype_declarator maybe_raises maybeasm maybe_attribute
			;

/* the * rules are dummies to accept the Apollo extended syntax
   so that the header files compile. */
maybe_attribute:
      /* empty */
  			| attributes
			;
 
attributes:
      attribute
			| attributes attribute
			;

attribute:
      ATTRIBUTE '(' '(' attribute_list ')' ')'
			;

attribute_list:
      attrib
			| attribute_list ',' attrib
			;
 
attrib:
    /* empty */
			| any_word
			| any_word '(' IDENTIFIER ')'
			| any_word '(' IDENTIFIER ',' nonnull_exprlist ')'
			| any_word '(' nonnull_exprlist ')'
			;

/* This still leaves out most reserved keywords,
   shouldn't we include them?  */

any_word:
	  identifier
	| SCSPEC
	| TYPESPEC
	| TYPE_QUAL
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
			| ENUM complex_type_name
		
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

specialization:
	  aggr template_type_name ';'
			;

named_class_head_sans_basetype:
	  aggr identifier
			| aggr complex_type_name
			| aggr template_type %prec EMPTY
			| aggr template_type_name '{'
		'{'	| aggr template_type_name ':'
			| specialization
	;

named_class_head_sans_basetype_defn:
	  aggr identifier_defn %prec EMPTY
			;

do_xref: /* empty */ %prec EMPTY
;	
do_xref_defn: /* empty */ %prec EMPTY
;
named_class_head:
	  named_class_head_sans_basetype do_xref
	  maybe_base_class_list %prec EMPTY
			|
	  named_class_head_sans_basetype_defn do_xref_defn
	  maybe_base_class_list %prec EMPTY
			;

unnamed_class_head: aggr '{'
		'{'	;

class_head: unnamed_class_head | named_class_head ;

maybe_base_class_list:
	  %prec EMPTY /* empty */
			| ':'  %prec EMPTY
			| ':' base_class_list  %prec EMPTY
			;

base_class_list:
	  base_class
	| base_class_list ',' base_class
			;

base_class:
	  base_class.1
			| base_class_access_list base_class.1
			;

base_class.1:
	  complete_type_name
	| SIGOF '(' expr ')'
			| SIGOF '(' type_id ')'
			;

base_class_access_list:
	  VISSPEC
	| SCSPEC
			| base_class_access_list VISSPEC
			| base_class_access_list SCSPEC
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
	  component_decl_1 ';'
	| component_decl_1 '}'
		'}'	/* C++: handle constructors, destructors and inline functions */
	/* note that INLINE is like a TYPESPEC */
	| fn.def2 ':' /* base_init compstmt */
			| fn.def2 '{' /* nodecls compstmt */
			;

component_decl_1:
	/* Do not add a "typed_declspecs declarator" rule here for
	   speed; we need to call grok_x_components for enums, so the
	   speedup would be insignificant.  */
	  typed_declspecs components
			| declmods notype_components
			| notype_declarator maybe_raises maybeasm maybe_attribute
			| ':' expr_no_commas
			| error
		
	/* These rules introduce a reduce/reduce conflict; in
		typedef int foo, bar;
		class A ;
	   should "A::foo" be declared as a function or "A::bar" as a data
	   member? In other words, is "bar" an after_type_declarator or a
	   parmlist? */
	| typed_declspecs '(' parmlist ')' type_quals
			| typed_declspecs LEFT_RIGHT type_quals
			;

/* The case of exactly one component is handled directly by component_decl. */
components:
	  /* empty: possibly anonymous */
			| component_declarator0
	| components ',' component_declarator
			;

notype_components:
	  /* empty: possibly anonymous */
			| notype_component_declarator0
	| notype_components ',' notype_component_declarator
			;

component_declarator0:
	  after_type_component_declarator0
	| notype_component_declarator0
	;

component_declarator:
	  after_type_component_declarator
	| notype_component_declarator
	;

after_type_component_declarator0:
	  after_type_declarator maybe_raises maybeasm maybe_attribute
			| after_type_declarator maybe_raises maybeasm maybe_attribute '=' init
			| TYPENAME ':' expr_no_commas maybe_attribute
			;

notype_component_declarator0:
	  notype_declarator maybe_raises maybeasm maybe_attribute
			| notype_declarator maybe_raises maybeasm maybe_attribute '=' init
			| IDENTIFIER ':' expr_no_commas maybe_attribute
			| ':' expr_no_commas maybe_attribute
			;

after_type_component_declarator:
	  after_type_declarator maybe_raises maybeasm maybe_attribute
			| after_type_declarator maybe_raises maybeasm maybe_attribute '=' init
			| TYPENAME ':' expr_no_commas maybe_attribute
			;

notype_component_declarator:
	  notype_declarator maybe_raises maybeasm maybe_attribute
			| notype_declarator maybe_raises maybeasm maybe_attribute '=' init
			| IDENTIFIER ':' expr_no_commas maybe_attribute
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

/* ANSI new-type-id (5.3.4) */
new_type_id:
	  type_specifier_seq new_declarator
			| type_specifier_seq %prec EMPTY
			/* GNU extension to allow arrays of arbitrary types with
	   non-constant dimension.  */
	| '(' type_id ')' '[' expr ']'
			;

type_quals:
	  /* empty */ %prec EMPTY
			| type_quals TYPE_QUAL
			;

nonempty_type_quals:
	  TYPE_QUAL
			| nonempty_type_quals TYPE_QUAL
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
	  '*' nonempty_type_quals after_type_declarator  %prec UNARY
			| '&' nonempty_type_quals after_type_declarator  %prec UNARY
			| '*' after_type_declarator  %prec UNARY
			| '&' after_type_declarator  %prec UNARY
			| ptr_to_mem type_quals after_type_declarator
			| direct_after_type_declarator
	;

qualified_type_name:
	  type_name %prec EMPTY
			| nested_type
	;

nested_type:
	nested_name_specifier type_name %prec EMPTY
			;

direct_after_type_declarator:
	  direct_after_type_declarator '(' nonnull_exprlist ')' type_quals %prec '.'
			| direct_after_type_declarator '(' parmlist ')' type_quals %prec '.'
			| direct_after_type_declarator LEFT_RIGHT type_quals %prec '.'
			| direct_after_type_declarator '(' error ')' type_quals %prec '.'
			| direct_after_type_declarator '[' nonmomentary_expr ']'
			| direct_after_type_declarator '[' ']'
			| '(' after_type_declarator ')'
			| nested_name_specifier type_name %prec EMPTY
			| type_name %prec EMPTY
	;

/* A declarator allowed whether or not there has been
   an explicit typespec.  These cannot redeclare a typedef-name.  */

notype_declarator:
	  '*' nonempty_type_quals notype_declarator  %prec UNARY
			| '&' nonempty_type_quals notype_declarator  %prec UNARY
			| '*' notype_declarator  %prec UNARY
			| '&' notype_declarator  %prec UNARY
			| ptr_to_mem type_quals notype_declarator
			| direct_notype_declarator
	;

complex_notype_declarator:
	  '*' nonempty_type_quals notype_declarator  %prec UNARY
			| '&' nonempty_type_quals notype_declarator  %prec UNARY
			| '*' complex_notype_declarator  %prec UNARY
			| '&' complex_notype_declarator  %prec UNARY
			| ptr_to_mem type_quals notype_declarator
			| complex_direct_notype_declarator
	;

complex_direct_notype_declarator:
	  direct_notype_declarator '(' nonnull_exprlist ')' type_quals  %prec '.'
			| direct_notype_declarator '(' parmlist ')' type_quals  %prec '.'
			| direct_notype_declarator LEFT_RIGHT type_quals  %prec '.'
			| direct_notype_declarator '(' error ')' type_quals  %prec '.'
			| '(' expr_or_declarator ')'
			| '(' complex_notype_declarator ')'
			| direct_notype_declarator '[' nonmomentary_expr ']'
			| direct_notype_declarator '[' ']'
			;

qualified_id:
	nested_name_specifier unqualified_id
			;

notype_qualified_id:
	nested_name_specifier notype_unqualified_id
			;

overqualified_id:
	  notype_qualified_id
	| global_scope notype_qualified_id
			;

functional_cast:
	  typespec '(' nonnull_exprlist ')'
			| typespec '(' expr_or_declarator ')'
			| typespec fcast_or_absdcl %prec EMPTY
			;

type_name:
	  TYPENAME
	| template_type %prec EMPTY
	;

nested_name_specifier:
	  nested_name_specifier_1
	| nested_name_specifier nested_name_specifier_1
			;

/* Why the @#$%^& do type_name and notype_identifier need to be expanded
   inline here?!?  (jason) */
nested_name_specifier_1:
	  TYPENAME SCOPE
			| template_type SCOPE
		/* 	These break 'const i;'
	| IDENTIFIER SCOPE
			| PTYPENAME SCOPE
		 */
	;

complete_type_name:
	  qualified_type_name
	| global_scope qualified_type_name
			;

complex_type_name:
	  nested_type
	| global_scope qualified_type_name
			;

ptr_to_mem:
	  nested_name_specifier '*'
			| global_scope nested_name_specifier '*'
			;

/* All uses of explicit global scope must go through this nonterminal so
   that got_scope will be set before yylex is called to get the next token. */
global_scope:
	  SCOPE
			;

/* ANSI new-declarator (5.3.4) */
new_declarator:
	  '*' type_quals new_declarator
			| '*' type_quals  %prec EMPTY
			| '&' type_quals new_declarator %prec EMPTY
			| '&' type_quals %prec EMPTY
			| ptr_to_mem type_quals %prec EMPTY
			| ptr_to_mem type_quals new_declarator
			| direct_new_declarator %prec EMPTY
	;

/* ANSI direct-new-declarator (5.3.4) */
direct_new_declarator:
	  '[' expr ']'
			| direct_new_declarator '[' nonmomentary_expr ']'
			;

/* ANSI abstract-declarator (8.1) */
absdcl:
	  '*' nonempty_type_quals absdcl
			| '*' absdcl
			| '*' nonempty_type_quals  %prec EMPTY
			| '*' %prec EMPTY
			| '&' nonempty_type_quals absdcl
			| '&' absdcl
			| '&' nonempty_type_quals %prec EMPTY
			| '&' %prec EMPTY
			| ptr_to_mem type_quals %prec EMPTY
			| ptr_to_mem type_quals absdcl
			| direct_abstract_declarator %prec EMPTY
	;

/* ANSI direct-abstract-declarator (8.1) */
direct_abstract_declarator:
	  '(' absdcl ')'
			  /* `(typedef)1' is `int'.  */
	| PAREN_STAR_PAREN
	| direct_abstract_declarator '(' parmlist ')' type_quals  %prec '.'
			| direct_abstract_declarator LEFT_RIGHT type_quals  %prec '.'
			| direct_abstract_declarator '[' nonmomentary_expr ']'  %prec '.'
			| direct_abstract_declarator '[' ']'  %prec '.'
			| '(' complex_parmlist ')' type_quals  %prec '.'
			| regcast_or_absdcl type_quals %prec '.'
			| fcast_or_absdcl type_quals %prec '.'
			| '[' nonmomentary_expr ']'  %prec '.'
			| '[' ']'  %prec '.'
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
			| '{' .pushlevel maybe_label_decls stmts error '}'
			| '{' .pushlevel maybe_label_decls error '}'
			;

simple_if:
	  IF
			  .pushlevel paren_cond_or_null
			  implicitly_scoped_stmt
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
			  implicitly_scoped_stmt
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
			  implicitly_scoped_stmt
			| CASE expr_no_commas ':'
			  stmt
	| CASE expr_no_commas ELLIPSIS expr_no_commas ':'
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
			| try_block
	;

try_block:
	  TRY '{' .pushlevel
			  ansi_try_stmts
			  handler_seq
			;

ansi_try_stmts:
	  '}'
		/* An empty try block is degenerate, but it's better to
		   do extra work here than to do all the special-case work
		   everywhere else.  */
			| stmts '}'
			| error '}'
			;

handler_seq:
	  /* empty */
	| handler_seq CATCH
			  handler_args compstmt
			;

type_specifier_seq:
	  typed_typespecs %prec EMPTY
	| nonempty_type_quals %prec EMPTY
	;

handler_args:
	  '(' ELLIPSIS ')'
			/* This doesn't allow reference parameters, the below does.
	| '(' type_specifier_seq absdcl ')'
			| '(' type_specifier_seq ')'
			| '(' type_specifier_seq notype_declarator ')'
			| '(' typed_typespecs after_type_declarator ')'
			*/
	| '(' parm ')'
			;

label_colon:
	  IDENTIFIER ':'
			| PTYPENAME ':'
			| TYPENAME ':'
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
			| complex_parmlist
	| type_id
			;

/* This nonterminal does not include the common sequence '(' type_id ')',
   as it is ambiguous and must be disambiguated elsewhere.  */
complex_parmlist:
	  parms
			| parms_comma ELLIPSIS
			/* C++ allows an ellipsis without a separating ',' */
	| parms ELLIPSIS
			| type_id ELLIPSIS
			| ELLIPSIS
			| TYPENAME_ELLIPSIS
			| parms TYPENAME_ELLIPSIS
			| type_id TYPENAME_ELLIPSIS
			| parms ':'
			| type_id ':'
			;

/* A nonempty list of parameter declarations or type names.  */
parms:
	  named_parm
			| parm '=' init
			| parms_comma full_parm
			| parms_comma bad_parm
			| parms_comma bad_parm '=' init
			;

parms_comma:
	  parms ','
	| type_id ','
			;

/* A single parameter declaration or parameter type name,
   as found in a parmlist.  The first four cases make up for 10%
   of the time spent parsing C++.  We cannot use them because
   of `int id[]' which won't get parsed properly.  */
named_parm:
/*
	  typed_declspecs dont_see_typename '*' IDENTIFIER
			| typed_declspecs dont_see_typename '&' IDENTIFIER
			| TYPENAME IDENTIFIER
			| TYPESPEC IDENTIFIER
			| */
	/* Here we expand typed_declspecs inline to avoid mis-parsing of
	   TYPESPEC IDENTIFIER.  */
	  typed_declspecs1 declarator
			| typed_typespecs declarator
			| typespec declarator
			| typed_declspecs1 absdcl
			| typed_declspecs1 %prec EMPTY
			| declmods notype_declarator
			;

full_parm:
	  parm
			| parm '=' init
			;

parm:
	named_parm
	| type_id
	;

see_typename: %prec EMPTY
		;

/* 
dont_see_typename: %prec EMPTY
		; 

try_for_typename:
        	;
*/

bad_parm:
	  /* empty */ %prec EMPTY
			| notype_declarator
			;

maybe_raises:
	  %prec EMPTY /* empty */
			| THROW '(' ansi_raise_identifiers  ')' %prec EMPTY
			;

ansi_raise_identifier:
	  type_id
			;

ansi_raise_identifiers:
	  ansi_raise_identifier
	| ansi_raise_identifiers ',' ansi_raise_identifier
			;

conversion_declarator:
	  /* empty */ %prec EMPTY
			| '*' type_quals conversion_declarator
			| '&' type_quals conversion_declarator
			| ptr_to_mem type_quals conversion_declarator
			;

operator: OPERATOR
			;

operator_name:
	  operator '*'
			| operator '/'
			| operator '%'
			| operator '+'
			| operator '-'
			| operator '&'
			| operator '|'
			| operator '^'
			| operator '~'
			| operator ','
			| operator ARITHCOMPARE
			| operator '<'
			| operator '>'
			| operator EQCOMPARE
			| operator ASSIGN
			| operator '='
			| operator LSHIFT
			| operator RSHIFT
			| operator PLUSPLUS
			| operator MINUSMINUS
			| operator ANDAND
			| operator OROR
			| operator '!'
			| operator '?' ':'
			| operator MIN_MAX
			| operator POINTSAT  %prec EMPTY
			| operator POINTSAT_STAR  %prec EMPTY
			| operator LEFT_RIGHT
			| operator '[' ']'
			| operator NEW %prec EMPTY
			| operator DELETE %prec EMPTY
			| operator NEW '[' ']'
			| operator DELETE '[' ']'
			/* Names here should be looked up in class scope ALSO.  */
	| operator type_specifier_seq conversion_declarator
			| operator error
			;

%%

#ifdef SPEW_DEBUG
const char *
debug_yytranslate (value)
    int value;

#endif
