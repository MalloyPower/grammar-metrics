/* YACC parser for C++ syntax.
   Copyright (C) 1988, 89, 93, 94, 95, 96, 1997 Free Software Foundation, Inc.
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* This grammar is based on the GNU CC grammar.  */

/* Note: Bison automatically applies a default action of "$$ = $1" for
   all derivations; this is applied before the explicit action, if one
   is given.  Keep this in mind when reading the actions.  */



%start program

 
/* All identifiers that are not reserved words
   and are not declared typedefs in the current block */
%token IDENTIFIER

/* All identifiers that are declared typedefs in the current block.
   In some contexts, they are treated just like IDENTIFIER,
   but they can also serve as typespecs in declarations.  */
%token TYPENAME
%token SELFNAME

/* A template function.  */
%token PFUNCNAME

/* Reserved words that specify storage class.
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token SCSPEC

/* Reserved words that specify type.
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token TYPESPEC

/* Reserved words that qualify type: "const" or "volatile".
   yylval contains an IDENTIFIER_NODE which indicates which one.  */
%token CV_QUALIFIER

/* Character or numeric constants.
   yylval is the node for the constant.  */
%token CONSTANT

/* String constants in raw form.
   yylval is a STRING_CST node.  */
%token STRING

/* "...", used for functions with variable arglists.  */
%token ELLIPSIS

/* the reserved words */
/* SCO include files test "ASM", so use something else.  */
%token SIZEOF ENUM /* STRUCT UNION */ IF ELSE WHILE DO FOR SWITCH CASE DEFAULT
%token BREAK CONTINUE RETURN GOTO ASM_KEYWORD GCC_ASM_KEYWORD TYPEOF ALIGNOF
%token SIGOF
%token ATTRIBUTE EXTENSION LABEL
%token REALPART IMAGPART

/* the reserved words... C++ extensions */
%token <ttype> AGGR
%token <ttype> VISSPEC
%token DELETE NEW THIS OPERATOR CXX_TRUE CXX_FALSE
%token NAMESPACE TYPENAME_KEYWORD USING
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

%left IDENTIFIER PFUNCNAME TYPENAME SELFNAME PTYPENAME SCSPEC TYPESPEC CV_QUALIFIER ENUM AGGR ELLIPSIS TYPEOF SIGOF OPERATOR NSNAME TYPENAME_KEYWORD

%left '{' ',' ';'

%nonassoc THROW
%right <code> ':'
%right <code> ASSIGN '='
%right <code> '?'
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
%nonassoc NEW DELETE TRY CATCH

%type <code> unop

%type <ttype> identifier IDENTIFIER TYPENAME CONSTANT expr nonnull_exprlist
%type <ttype> PFUNCNAME
%type <ttype> paren_expr_or_null nontrivial_exprlist SELFNAME
%type <ttype> expr_no_commas cast_expr unary_expr primary string STRING
%type <ttype> reserved_declspecs boolean.literal
%type <ttype> reserved_typespecquals
%type <ttype> declmods 
%type <ttype> SCSPEC TYPESPEC CV_QUALIFIER maybe_cv_qualifier
%type <itype> initdecls notype_initdecls initdcl	/* C++ modification */
%type <ttype> init initlist maybeasm maybe_init defarg defarg1
%type <ttype> asm_operands nonnull_asm_operands asm_operand asm_clobbers
%type <ttype> maybe_attribute attributes attribute attribute_list attrib
%type <ttype> any_word

%type <ttype> compstmt implicitly_scoped_stmt

%type <ttype> declarator notype_declarator after_type_declarator
%type <ttype> direct_notype_declarator direct_after_type_declarator

%type <ttype> opt.component_decl_list component_decl_list
%type <ttype> component_decl component_decl_1 components notype_components
%type <ttype> component_declarator component_declarator0 self_reference
%type <ttype> notype_component_declarator notype_component_declarator0
%type <ttype> after_type_component_declarator after_type_component_declarator0
%type <ttype> enumlist enumerator
%type <ttype> absdcl cv_qualifiers
%type <ttype> direct_abstract_declarator conversion_declarator
%type <ttype> new_declarator direct_new_declarator
%type <ttype> xexpr parmlist parms bad_parm 
%type <ttype> identifiers_or_typenames
%type <ttype> fcast_or_absdcl regcast_or_absdcl
%type <ttype> expr_or_declarator complex_notype_declarator
%type <ttype> notype_unqualified_id unqualified_id qualified_id
%type <ttype> template_id object_template_id notype_template_declarator
%type <ttype> overqualified_id notype_qualified_id any_id
%type <ttype> complex_direct_notype_declarator functional_cast
%type <ttype> complex_parmlist parms_comma

%type <ftype> type_id new_type_id typed_typespecs typespec typed_declspecs
%type <ftype> typed_declspecs1 type_specifier_seq nonempty_cv_qualifiers
%type <ftype> structsp typespecqual_reserved parm named_parm full_parm

/* C++ extensions */
%token <ttype> TYPENAME_ELLIPSIS PTYPENAME
%token <ttype> PRE_PARSED_FUNCTION_DECL EXTERN_LANG_STRING ALL
%token <ttype> PRE_PARSED_CLASS_DECL DEFARG DEFARG_MARKER
%type <ttype> fn.def1 /* Not really! */ component_constructor_declarator
%type <ttype> fn.def2 return_id fn.defpen constructor_declarator
%type <itype> ctor_initializer_opt
%type <ttype> named_class_head named_class_head_sans_basetype
%type <ttype> named_complex_class_head_sans_basetype
%type <ttype> unnamed_class_head
%type <ttype> class_head base_class_list
%type <ttype> base_class_access_list
%type <ttype> base_class maybe_base_class_list base_class.1
%type <ttype> exception_specification_opt ansi_raise_identifier ansi_raise_identifiers
%type <ttype> operator_name
%type <ttype> object aggr
%type <itype> new delete
/* %type <ttype> primary_no_id */
%type <ttype> nonmomentary_expr maybe_parmlist
%type <itype> initdcl0 notype_initdcl0 member_init_list
%type <ttype> template_header template_parm_list template_parm
%type <ttype> template_type_parm
%type <code>  template_close_bracket
%type <ttype> template_type template_arg_list template_arg
%type <ttype> condition xcond paren_cond_or_null
%type <ttype> type_name nested_name_specifier nested_type ptr_to_mem
%type <ttype> complete_type_name notype_identifier nonnested_type
%type <ttype> complex_type_name nested_name_specifier_1
%type <itype> nomods_initdecls nomods_initdcl0
%type <ttype> new_initializer new_placement
%type <ttype> using_decl .poplevel
%type <ttype> typename_sub typename_sub0 typename_sub1 typename_sub2
/* in order to recognize aggr tags as defining and thus shadowing.  */
%token TYPENAME_DEFN IDENTIFIER_DEFN PTYPENAME_DEFN
%type <ttype> named_class_head_sans_basetype_defn
%type <ttype> identifier_defn IDENTIFIER_DEFN TYPENAME_DEFN PTYPENAME_DEFN

%type <ttype> self_template_type

%token NSNAME
%type <ttype> NSNAME

/* Used in lex.c for parsing pragmas.  */
%token END_OF_LINE

/* lex.c and pt.c depend on this being the last token.  Define
   any new tokens before this one!  */
%token END_OF_SAVED_INPUT



%%
program:
	  /* empty */
	| extdefs
			;

/* the reason for the strange actions in this rule
 is so that notype_initdecls when reached via datadef
 can find a valid list of type and sc specs in $0.  */

extdefs:
			  lang_extdef
			| extdefs lang_extdef
			;

extdefs_opt:
	  extdefs
	| /* empty */
	;

.hush_warning:
			;
.warning_ok:
			;

extension:
	EXTENSION
			;

asm_keyword:
	  ASM_KEYWORD
	| GCC_ASM_KEYWORD
	;

lang_extdef:
			  extdef
			;

extdef:
	  fndef eat_saved_input
			| datadef
			| template_def
			| asm_keyword '(' string ')' ';'
			| extern_lang_string '{' extdefs_opt '}'
			| extern_lang_string .hush_warning fndef .warning_ok eat_saved_input
			| extern_lang_string .hush_warning datadef .warning_ok
			| NAMESPACE identifier '{'
			  extdefs_opt '}'
			| NAMESPACE '{'
			  extdefs_opt '}'
			| NAMESPACE identifier '=' any_id ';'
			| using_decl ';'
			| USING NAMESPACE any_id ';'
			| extension extdef
			;

using_decl:
	  USING qualified_id
			| USING global_scope qualified_id
			| USING global_scope unqualified_id
			;

any_id:
	  unqualified_id
	| qualified_id
	| global_scope qualified_id
			| global_scope unqualified_id
			;

extern_lang_string:
	EXTERN_LANG_STRING
			| extern_lang_string EXTERN_LANG_STRING
			;

template_header:
	  TEMPLATE '<'
			  template_parm_list '>'
			| TEMPLATE '<' '>'
                	;

template_parm_list:
	  template_parm
			| template_parm_list ',' template_parm
			;

template_type_parm:
	  aggr
			| aggr identifier
			| TYPENAME_KEYWORD
			| TYPENAME_KEYWORD identifier
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
			| template_type_parm '=' type_id
			| parm
			| parm '=' expr_no_commas  %prec ARITHCOMPARE
			;

template_def:
	  template_header
	  extdef
                	| template_header
	  error  %prec EMPTY
			;

datadef:
	  nomods_initdecls ';'
			| declmods notype_initdecls ';'
			| typed_declspecs initdecls ';'
		        | declmods ';'
			| explicit_instantiation ';'
	| typed_declspecs ';'
			| error ';'
	| error '}'
	| ';'
	;

ctor_initializer_opt:
	  nodecls
			| base_init
			;

maybe_return_init:
	  /* empty */
	| return_init
	| return_init ';'
	;

eat_saved_input:
	  /* empty */
	| END_OF_SAVED_INPUT
	;

fndef:
	  fn.def1 maybe_return_init ctor_initializer_opt compstmt_or_error
			| fn.def1 maybe_return_init function_try_block
			| fn.def1 maybe_return_init error
			;

constructor_declarator:
	  nested_name_specifier SELFNAME '(' 
			  parmlist ')' cv_qualifiers exception_specification_opt
			| nested_name_specifier SELFNAME LEFT_RIGHT cv_qualifiers exception_specification_opt
			| global_scope nested_name_specifier SELFNAME '(' 
			 parmlist ')' cv_qualifiers exception_specification_opt
			| global_scope nested_name_specifier SELFNAME LEFT_RIGHT cv_qualifiers exception_specification_opt
			| nested_name_specifier self_template_type '(' 
			  parmlist ')' cv_qualifiers exception_specification_opt
			| nested_name_specifier self_template_type LEFT_RIGHT cv_qualifiers exception_specification_opt
			| global_scope nested_name_specifier self_template_type '(' 
			 parmlist ')' cv_qualifiers exception_specification_opt
			| global_scope nested_name_specifier self_template_type LEFT_RIGHT cv_qualifiers exception_specification_opt
			;

fn.def1:
	  typed_declspecs declarator
			| declmods notype_declarator
			| notype_declarator
			| declmods constructor_declarator
			| constructor_declarator
			;

component_constructor_declarator:
	  SELFNAME '(' parmlist ')' cv_qualifiers exception_specification_opt
			| SELFNAME LEFT_RIGHT cv_qualifiers exception_specification_opt
			| self_template_type '(' parmlist ')' cv_qualifiers exception_specification_opt
			| self_template_type LEFT_RIGHT cv_qualifiers exception_specification_opt
			;

/* more C++ complexity.  See component_decl for a comment on the
   reduce/reduce conflict introduced by these rules.  */
fn.def2:
	  declmods component_constructor_declarator
			| component_constructor_declarator
			| typed_declspecs declarator
			| declmods notype_declarator
			| notype_declarator
			| declmods constructor_declarator
			| constructor_declarator
		        | template_header fn.def2 
                	;

return_id:
	  RETURN IDENTIFIER
			;

return_init:
	  return_id maybe_init
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

member_init:
	  '(' nonnull_exprlist ')'
			| LEFT_RIGHT
			| notype_identifier '(' nonnull_exprlist ')'
			| notype_identifier LEFT_RIGHT
			| nonnested_type '(' nonnull_exprlist ')'
			| nonnested_type LEFT_RIGHT
			| typename_sub '(' nonnull_exprlist ')'
			| typename_sub LEFT_RIGHT
			;

identifier:
	  IDENTIFIER
	| TYPENAME
	| SELFNAME
	| PTYPENAME
	| NSNAME
	;

notype_identifier:
	  IDENTIFIER
	| PTYPENAME 
	| NSNAME  %prec EMPTY
	;

identifier_defn:
	  IDENTIFIER_DEFN
	| TYPENAME_DEFN
	| PTYPENAME_DEFN
	;

explicit_instantiation:
	  TEMPLATE typespec ';'
			| TEMPLATE typed_declspecs declarator
			| TEMPLATE notype_declarator
			| TEMPLATE constructor_declarator
			| SCSPEC TEMPLATE typespec ';'
			| SCSPEC TEMPLATE typed_declspecs declarator
			| SCSPEC TEMPLATE notype_declarator
			| SCSPEC TEMPLATE constructor_declarator
			;

/* The TYPENAME expansions are to deal with use of a template class name as
  a template within the class itself, where the template decl is hidden by
  a type decl.  Got all that?  */

template_type:
	  PTYPENAME '<' template_arg_list template_close_bracket
			| PTYPENAME '<' template_close_bracket
			| TYPENAME  '<' template_arg_list template_close_bracket
			| TYPENAME '<' template_close_bracket
			| self_template_type
	;

self_template_type:
	  SELFNAME  '<' template_arg_list template_close_bracket
			| SELFNAME '<' template_close_bracket
			;

template_close_bracket:
	  '>'
	| RSHIFT 
			;

template_arg_list:
	  template_arg
			| template_arg_list ',' template_arg
			;

template_arg:
	  type_id
			| expr_no_commas  %prec ARITHCOMPARE
	;

unop:
	  '-'
			| '+'
			| PLUSPLUS
			| MINUSMINUS
			| '!'
			;

expr:
	  nontrivial_exprlist
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
	  type_specifier_seq declarator maybeasm maybe_attribute '='
			  init
			| expr
	;

compstmtend:
	  '}'
	| maybe_label_decls stmts '}'
	| maybe_label_decls stmts error '}'
	| maybe_label_decls error '}'
	;

already_scoped_stmt:
	  '{'
			  compstmtend
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
	  primary  %prec UNARY
			/* __extension__ turns off -pedantic for following primary.  */
	| extension cast_expr  	  %prec UNARY
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
	| new new_type_id  %prec EMPTY
			| new new_type_id new_initializer
			| new new_placement new_type_id  %prec EMPTY
			| new new_placement new_type_id new_initializer
			| new '(' type_id ')'  %prec EMPTY
			| new '(' type_id ')' new_initializer
			| new new_placement '(' type_id ')'  %prec EMPTY
			| new new_placement '(' type_id ')' new_initializer
		
	| delete cast_expr  %prec UNARY
			| delete '[' ']' cast_expr  %prec UNARY
			| delete '[' expr ']' cast_expr  %prec UNARY
			| REALPART cast_expr %prec UNARY
			| IMAGPART cast_expr %prec UNARY
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
	  '(' type_id ')'  %prec EMPTY
			| regcast_or_absdcl '(' type_id ')'  %prec EMPTY
			;

cast_expr:
	  unary_expr
	| regcast_or_absdcl unary_expr  %prec UNARY
			| regcast_or_absdcl '{' initlist maybecomma '}'  %prec UNARY
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
	| object '&' expr_no_commas  %prec UNARY
			| object unop expr_no_commas  %prec UNARY
			| object '(' type_id ')' expr_no_commas  %prec UNARY
			| object primary_no_id  %prec UNARY
		*/
	;

notype_unqualified_id:
	  '~' see_typename identifier
		        | template_id
	| operator_name
	| IDENTIFIER
	| PTYPENAME
	| NSNAME  %prec EMPTY
	;

template_id:
        PFUNCNAME '<' template_arg_list template_close_bracket 
                        | PFUNCNAME '<' template_close_bracket
                        | operator_name '<' template_arg_list template_close_bracket
                        | operator_name '<' template_close_bracket
                	;

object_template_id:
        TEMPLATE identifier '<' template_arg_list template_close_bracket
                        | TEMPLATE PFUNCNAME '<' template_arg_list template_close_bracket
                        | TEMPLATE operator_name '<' template_arg_list template_close_bracket
                        ;

unqualified_id:
	  notype_unqualified_id
	| TYPENAME
	| SELFNAME
	;

expr_or_declarator:
	  notype_unqualified_id
	| '*' expr_or_declarator  %prec UNARY
			| '&' expr_or_declarator  %prec UNARY
			| '(' expr_or_declarator ')'
			;

notype_template_declarator:
	  IDENTIFIER '<' template_arg_list template_close_bracket
                	| NSNAME '<' template_arg_list template_close_bracket
                	;
		
direct_notype_declarator:
	  complex_direct_notype_declarator
	| notype_unqualified_id
	| notype_template_declarator
	| '(' expr_or_declarator ')'
			;

primary:
	  notype_unqualified_id
				
	| CONSTANT
	| boolean.literal
	| string
			| '(' expr ')'
			| '(' expr_or_declarator ')'
			| '(' error ')'
			| '('
			  compstmt ')'
			| primary '(' nonnull_exprlist ')'
                	| primary LEFT_RIGHT
                	| primary '[' expr ']'
			| primary PLUSPLUS
			| primary MINUSMINUS
			/* C++ extensions */
	| THIS
			| CV_QUALIFIER '(' nonnull_exprlist ')'
			| functional_cast
	| DYNAMIC_CAST '<' type_id '>' '(' expr ')'
			| STATIC_CAST '<' type_id '>' '(' expr ')'
			| REINTERPRET_CAST '<' type_id '>' '(' expr ')'
			| CONST_CAST '<' type_id '>' '(' expr ')'
			| TYPEID '(' expr ')'
			| TYPEID '(' type_id ')'
			| global_scope IDENTIFIER
			| global_scope operator_name
			| overqualified_id  %prec HYPERUNARY
			| overqualified_id '(' nonnull_exprlist ')'
			| overqualified_id LEFT_RIGHT
		        | object object_template_id %prec UNARY
                        | object object_template_id '(' nonnull_exprlist ')'
                	| object object_template_id LEFT_RIGHT
                	| object unqualified_id  %prec UNARY
			| object overqualified_id  %prec UNARY
			| object unqualified_id '(' nonnull_exprlist ')'
			| object unqualified_id LEFT_RIGHT
			| object overqualified_id '(' nonnull_exprlist ')'
			| object overqualified_id LEFT_RIGHT
			/* p->int::~int() is valid -- 12.4 */
	| object '~' TYPESPEC LEFT_RIGHT
			| object TYPESPEC SCOPE '~' TYPESPEC LEFT_RIGHT
			| object error
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

new:
	  NEW
			| global_scope NEW
			;

delete:
	  DELETE
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

object:
	  primary '.'
			| primary POINTSAT
			;

decl:
	  typespec initdecls ';'
			| typed_declspecs initdecls ';'
			| declmods notype_initdecls ';'
			| typed_declspecs ';'
			| declmods ';'
			| extension decl
			;

/* Any kind of declarator (thus, all declarators allowed
   after an explicit typespec).  */

declarator:
	  after_type_declarator  %prec EMPTY
	| notype_declarator  %prec EMPTY
	;

/* This is necessary to postpone reduction of `int()()()()'.  */
fcast_or_absdcl:
	  LEFT_RIGHT  %prec EMPTY
			| fcast_or_absdcl LEFT_RIGHT  %prec EMPTY
			;

/* ANSI type-id (8.1) */
type_id:
	  typed_typespecs absdcl
			| nonempty_cv_qualifiers absdcl
			| typespec absdcl
			| typed_typespecs  %prec EMPTY
			| nonempty_cv_qualifiers  %prec EMPTY
			;

/* Declspecs which contain at least one type specifier or typedef name.
   (Just `const' or `volatile' is not enough.)
   A typedef'd name following these is taken as a name to be declared.
   In the result, declspecs have a non-NULL TREE_VALUE, attributes do not.  */

typed_declspecs:
	  typed_typespecs  %prec EMPTY
	| typed_declspecs1
	;

typed_declspecs1:
	  declmods typespec
			| typespec reserved_declspecs  %prec HYPERUNARY
			| typespec reserved_typespecquals reserved_declspecs
			| declmods typespec reserved_declspecs
			| declmods typespec reserved_typespecquals
			| declmods typespec reserved_typespecquals reserved_declspecs
			;

reserved_declspecs:
	  SCSPEC
			| reserved_declspecs typespecqual_reserved
			| reserved_declspecs SCSPEC
			| reserved_declspecs attributes
			| attributes
			;

/* List of just storage classes and type modifiers.
   A declaration can start with just this, but then it cannot be used
   to redeclare a typedef-name.
   In the result, declspecs have a non-NULL TREE_VALUE, attributes do not.  */

declmods:
	  nonempty_cv_qualifiers  %prec EMPTY
			| SCSPEC
			| declmods CV_QUALIFIER
			| declmods SCSPEC
			| declmods attributes
			| attributes
			;

/* Used instead of declspecs where storage classes are not allowed
   (that is, for typenames and structure components).

   C++ can takes storage classes for structure components.
   Don't accept a typedef-name if anything but a modifier precedes it.  */

typed_typespecs:
	  typespec  %prec EMPTY
			| nonempty_cv_qualifiers typespec
			| typespec reserved_typespecquals
			| nonempty_cv_qualifiers typespec reserved_typespecquals
			;

reserved_typespecquals:
	  typespecqual_reserved
			| reserved_typespecquals typespecqual_reserved
			;

/* A typespec (but not a type qualifier).
   Once we have seen one of these in a declaration,
   if a typedef name appears then it is being redeclared.  */

typespec:
	  structsp
	| TYPESPEC  %prec EMPTY
			| complete_type_name
			| TYPEOF '(' expr ')'
			| TYPEOF '(' type_id ')'
			| SIGOF '(' expr ')'
			| SIGOF '(' type_id ')'
			;

/* A typespec that is a reserved word, or a type qualifier.  */

typespecqual_reserved:
	  TYPESPEC
			| CV_QUALIFIER
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
	  declarator maybeasm maybe_attribute '='
			  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
			| declarator maybeasm maybe_attribute
			;

initdcl:
	  declarator maybeasm maybe_attribute '='
			  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
			| declarator maybeasm maybe_attribute
			;

notype_initdcl0:
	  notype_declarator maybeasm maybe_attribute '='
			  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
			| notype_declarator maybeasm maybe_attribute
			;

nomods_initdcl0:
	  notype_declarator maybeasm maybe_attribute '='
			  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
			| notype_declarator maybeasm maybe_attribute
			;

/* the * rules are dummies to accept the Apollo extended syntax
   so that the header files compile.  */
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
	| CV_QUALIFIER
	;

/* A nonempty list of identifiers, including typenames.  */
identifiers_or_typenames:
	  identifier
			| identifiers_or_typenames ',' identifier
			;

maybe_init:
	  /* empty */  %prec EMPTY
			| '=' init
	  ;
/* If we are processing a template, we don't want to expand this
   initializer yet.  */

init:
	  expr_no_commas  %prec '='
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

fn.defpen:
	PRE_PARSED_FUNCTION_DECL
;
pending_inline:
	  fn.defpen maybe_return_init ctor_initializer_opt compstmt_or_error
			| fn.defpen maybe_return_init function_try_block
			| fn.defpen maybe_return_init error
			;

pending_inlines:
	/* empty */
	| pending_inlines pending_inline eat_saved_input
	;

/* A regurgitated default argument.  The value of DEFARG_MARKER will be
   the TREE_LIST node for the parameter in question.  */
defarg_again:
	DEFARG_MARKER expr_no_commas END_OF_SAVED_INPUT
			| DEFARG_MARKER error END_OF_SAVED_INPUT
	;
pending_defargs:
	  /* empty */ %prec EMPTY
	| pending_defargs defarg_again
			| pending_defargs error
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
			| TYPENAME_KEYWORD typename_sub
			/* C++ extensions, merged with C to avoid shift/reduce conflicts */
	| class_head left_curly 
                          opt.component_decl_list '}' maybe_attribute
			  pending_defargs
			  pending_inlines
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

aggr:
	  AGGR
	| aggr SCSPEC
			| aggr TYPESPEC
			| aggr CV_QUALIFIER
			| aggr AGGR
			;

named_class_head_sans_basetype:
	  aggr identifier
			;

named_class_head_sans_basetype_defn:
	  aggr identifier_defn  %prec EMPTY
			;

named_complex_class_head_sans_basetype:
	  aggr nested_name_specifier identifier
			| aggr global_scope nested_name_specifier identifier
			| aggr global_scope identifier
			| aggr template_type
			| aggr nested_name_specifier template_type
			;

do_xref_defn:
	  /* empty */  %prec EMPTY
			;

named_class_head:
	  named_class_head_sans_basetype  %prec EMPTY
			| named_class_head_sans_basetype_defn do_xref_defn
          maybe_base_class_list  %prec EMPTY
			| named_complex_class_head_sans_basetype maybe_base_class_list
			;

unnamed_class_head:
	  aggr '{'
		'{'	;

class_head:
	  unnamed_class_head
	| named_class_head
	;

maybe_base_class_list:
	  /* empty */  %prec EMPTY
			| ':' see_typename  %prec EMPTY
			| ':' see_typename base_class_list  %prec EMPTY
			;

base_class_list:
	  base_class
	| base_class_list ',' see_typename base_class
			;

base_class:
	  base_class.1
			| base_class_access_list see_typename base_class.1
			;

base_class.1:
	  typename_sub
			| nonnested_type
	| SIGOF '(' expr ')'
			| SIGOF '(' type_id ')'
			;

base_class_access_list:
	  VISSPEC see_typename
	| SCSPEC see_typename
			| base_class_access_list VISSPEC see_typename
			| base_class_access_list SCSPEC see_typename
			;

left_curly:
	  '{'
			;

self_reference:
	  /* empty */
			;

opt.component_decl_list:
	  self_reference
			| self_reference component_decl_list
			| opt.component_decl_list VISSPEC ':' component_decl_list
			| opt.component_decl_list VISSPEC ':'
			;

/* Note: we no longer warn about the semicolon after a component_decl_list.
   ARM $9.2 says that the semicolon is optional, and therefore allowed.  */
component_decl_list:
	  component_decl
			| component_decl_list component_decl
			;

component_decl:
	  component_decl_1 ';'
			| component_decl_1 '}'
		'}'	/* C++: handle constructors, destructors and inline functions */
	/* note that INLINE is like a TYPESPEC */
	| fn.def2 ':' /* base_init compstmt */
			| fn.def2 TRY /* base_init compstmt */
			| fn.def2 RETURN /* base_init compstmt */
			| fn.def2 '{' /* nodecls compstmt */
			| ';'
			| extension component_decl
			;

component_decl_1:
	/* Do not add a "typed_declspecs declarator" rule here for
	   speed; we need to call grok_x_components for enums, so the
	   speedup would be insignificant.  */
	  typed_declspecs components
			| declmods notype_components
			| notype_declarator maybeasm maybe_attribute maybe_init
			| constructor_declarator maybeasm maybe_attribute maybe_init
			| ':' expr_no_commas
			| error
		
	/* These rules introduce a reduce/reduce conflict; in
		typedef int foo, bar;
		class A ;
	   should "A::foo" be declared as a function or "A::bar" as a data
	   member? In other words, is "bar" an after_type_declarator or a
	   parmlist? */
	| declmods component_constructor_declarator maybeasm maybe_attribute maybe_init
			| component_constructor_declarator maybeasm maybe_attribute maybe_init
			| using_decl
		        | template_header component_decl_1 
	  ;
/* The case of exactly one component is handled directly by component_decl.  */
/* ??? Huh? ^^^ */
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
	  after_type_declarator maybeasm maybe_attribute maybe_init
			| TYPENAME ':' expr_no_commas maybe_attribute
			;

notype_component_declarator0:
	  notype_declarator maybeasm maybe_attribute maybe_init
			| constructor_declarator maybeasm maybe_attribute maybe_init
			| IDENTIFIER ':' expr_no_commas maybe_attribute
			| ':' expr_no_commas maybe_attribute
			;

after_type_component_declarator:
	  after_type_declarator maybeasm maybe_attribute maybe_init
			| TYPENAME ':' expr_no_commas maybe_attribute
			;

notype_component_declarator:
	  notype_declarator maybeasm maybe_attribute maybe_init
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
			| type_specifier_seq  %prec EMPTY
			/* GNU extension to allow arrays of arbitrary types with
	   non-constant dimension.  */
	| '(' type_id ')' '[' expr ']'
			;

cv_qualifiers:
	  /* empty */  %prec EMPTY
			| cv_qualifiers CV_QUALIFIER
			;

nonempty_cv_qualifiers:
	  CV_QUALIFIER
			| nonempty_cv_qualifiers CV_QUALIFIER
			;

/* These rules must follow the rules for function declarations
   and component declarations.  That way, longer rules are preferred.  */

suspend_mom:
	  /* empty */
	  ; 

/* An expression which will not live on the momentary obstack.  */
nonmomentary_expr:
	  suspend_mom expr
			;

/* An expression which will not live on the momentary obstack.  */
maybe_parmlist:
	  suspend_mom '(' nonnull_exprlist ')'
			| suspend_mom '(' parmlist ')'
			| suspend_mom LEFT_RIGHT
			| suspend_mom '(' error ')'
			;

/* A declarator that is allowed only after an explicit typespec.  */
/* may all be followed by prec '.' */
after_type_declarator:
	  '*' nonempty_cv_qualifiers after_type_declarator  %prec UNARY
			| '&' nonempty_cv_qualifiers after_type_declarator  %prec UNARY
			| '*' after_type_declarator  %prec UNARY
			| '&' after_type_declarator  %prec UNARY
			| ptr_to_mem cv_qualifiers after_type_declarator
			| direct_after_type_declarator
	;

nonnested_type:
	  type_name  %prec EMPTY
			| global_scope type_name
			;

complete_type_name:
	  nonnested_type
	| nested_type
	| global_scope nested_type
			;

nested_type:
	  nested_name_specifier type_name  %prec EMPTY
			;

direct_after_type_declarator:
	  direct_after_type_declarator maybe_parmlist cv_qualifiers exception_specification_opt  %prec '.'
			| direct_after_type_declarator '[' nonmomentary_expr ']'
			| direct_after_type_declarator '[' ']'
			| '(' after_type_declarator ')'
			| nested_name_specifier type_name  %prec EMPTY
			| type_name  %prec EMPTY
	;

/* A declarator allowed whether or not there has been
   an explicit typespec.  These cannot redeclare a typedef-name.  */

notype_declarator:
	  '*' nonempty_cv_qualifiers notype_declarator  %prec UNARY
			| '&' nonempty_cv_qualifiers notype_declarator  %prec UNARY
			| '*' notype_declarator  %prec UNARY
			| '&' notype_declarator  %prec UNARY
			| ptr_to_mem cv_qualifiers notype_declarator
			| direct_notype_declarator
	;

complex_notype_declarator:
	  '*' nonempty_cv_qualifiers notype_declarator  %prec UNARY
			| '&' nonempty_cv_qualifiers notype_declarator  %prec UNARY
			| '*' complex_notype_declarator  %prec UNARY
			| '&' complex_notype_declarator  %prec UNARY
			| ptr_to_mem cv_qualifiers notype_declarator
			| complex_direct_notype_declarator
	;

complex_direct_notype_declarator:
	  direct_notype_declarator maybe_parmlist cv_qualifiers exception_specification_opt  %prec '.'
			| '(' complex_notype_declarator ')'
			| direct_notype_declarator '[' nonmomentary_expr ']'
			| direct_notype_declarator '[' ']'
			| notype_qualified_id
		        | nested_name_specifier notype_template_declarator
                	;

qualified_id:
	  nested_name_specifier unqualified_id
		        | nested_name_specifier object_template_id
                	;

notype_qualified_id:
	  nested_name_specifier notype_unqualified_id
		        | nested_name_specifier object_template_id
                	;

overqualified_id:
	  notype_qualified_id
	| global_scope notype_qualified_id
			;

functional_cast:
	  typespec '(' nonnull_exprlist ')'
			| typespec '(' expr_or_declarator ')'
			| typespec fcast_or_absdcl  %prec EMPTY
			;

type_name:
	  TYPENAME
	| SELFNAME
	| template_type  %prec EMPTY
	;

nested_name_specifier:
	  nested_name_specifier_1
	| nested_name_specifier nested_name_specifier_1
			;

/* Why the @#$%^& do type_name and notype_identifier need to be expanded
   inline here?!?  (jason) */
nested_name_specifier_1:
	  TYPENAME SCOPE
			| SELFNAME SCOPE
			| NSNAME SCOPE
			| template_type SCOPE
		/* 	These break 'const i;'
	| IDENTIFIER SCOPE
			| PTYPENAME SCOPE
		 */
	;

typename_sub:
	  typename_sub0
	| global_scope typename_sub0
			;

typename_sub0:
	  typename_sub1 identifier
			;

typename_sub1:
	  typename_sub2
			| typename_sub1 typename_sub2
			;

typename_sub2:
	  TYPENAME SCOPE
			| SELFNAME SCOPE
			| template_type SCOPE
			| PTYPENAME SCOPE
	| IDENTIFIER SCOPE
	| NSNAME SCOPE
			;

complex_type_name:
	  global_scope type_name
			| nested_type
	| global_scope nested_type
			;

ptr_to_mem:
	  nested_name_specifier '*'
			| global_scope nested_name_specifier '*'
			;

/* All uses of explicit global scope must go through this nonterminal so
   that got_scope will be set before yylex is called to get the next token.  */
global_scope:
	  SCOPE
			;

/* ANSI new-declarator (5.3.4) */
new_declarator:
	  '*' cv_qualifiers new_declarator
			| '*' cv_qualifiers  %prec EMPTY
			| '&' cv_qualifiers new_declarator  %prec EMPTY
			| '&' cv_qualifiers  %prec EMPTY
			| ptr_to_mem cv_qualifiers  %prec EMPTY
			| ptr_to_mem cv_qualifiers new_declarator
			| direct_new_declarator  %prec EMPTY
	;

/* ANSI direct-new-declarator (5.3.4) */
direct_new_declarator:
	  '[' expr ']'
			| direct_new_declarator '[' nonmomentary_expr ']'
			;

/* ANSI abstract-declarator (8.1) */
absdcl:
	  '*' nonempty_cv_qualifiers absdcl
			| '*' absdcl
			| '*' nonempty_cv_qualifiers  %prec EMPTY
			| '*'  %prec EMPTY
			| '&' nonempty_cv_qualifiers absdcl
			| '&' absdcl
			| '&' nonempty_cv_qualifiers  %prec EMPTY
			| '&'  %prec EMPTY
			| ptr_to_mem cv_qualifiers  %prec EMPTY
			| ptr_to_mem cv_qualifiers absdcl
			| direct_abstract_declarator  %prec EMPTY
	;

/* ANSI direct-abstract-declarator (8.1) */
direct_abstract_declarator:
	  '(' absdcl ')'
			  /* `(typedef)1' is `int'.  */
	| PAREN_STAR_PAREN
	| direct_abstract_declarator '(' parmlist ')' cv_qualifiers exception_specification_opt  %prec '.'
			| direct_abstract_declarator LEFT_RIGHT cv_qualifiers exception_specification_opt  %prec '.'
			| direct_abstract_declarator '[' nonmomentary_expr ']'  %prec '.'
			| direct_abstract_declarator '[' ']'  %prec '.'
			| '(' complex_parmlist ')' cv_qualifiers exception_specification_opt  %prec '.'
			| regcast_or_absdcl cv_qualifiers exception_specification_opt  %prec '.'
			| fcast_or_absdcl cv_qualifiers exception_specification_opt  %prec '.'
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

errstmt:
	  error ';'
	;

/* build the LET_STMT node before parsing its contents,
  so that any LET_STMTs within the context can have their display pointers
  set up to point at this one.  */

.pushlevel:
	  /* empty */
			;

.poplevel:
	  /* empty */
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

compstmt:
	  '{'
			  .pushlevel compstmtend .poplevel
			;

simple_if:
	  IF
			  .pushlevel paren_cond_or_null
			  implicitly_scoped_stmt
			;

implicitly_scoped_stmt:
	  compstmt
			| .pushlevel
			  simple_stmt .poplevel
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
			  .poplevel
			| simple_if  %prec IF
			| WHILE
			  .pushlevel paren_cond_or_null
			  already_scoped_stmt .poplevel
			| DO
			  implicitly_scoped_stmt WHILE
			  paren_expr_or_null ';'
			| FOR
			  '(' for.init.statement
			  .pushlevel xcond ';'
			  xexpr ')'
		/* Don't let the tree nodes for $10 be discarded
		   by clear_momentary during the parsing of the next stmt.  */
			  already_scoped_stmt .poplevel
			| SWITCH .pushlevel '(' condition ')'
			  implicitly_scoped_stmt
			  .poplevel
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
			| asm_keyword maybe_cv_qualifier '(' string ')' ';'
			/* This is the case with just output operands.  */
	| asm_keyword maybe_cv_qualifier '(' string ':' asm_operands ')' ';'
			/* This is the case with input operands as well.  */
	| asm_keyword maybe_cv_qualifier '(' string ':' asm_operands ':' asm_operands ')' ';'
			/* This is the case with clobbered registers as well.  */
	| asm_keyword maybe_cv_qualifier '(' string ':' asm_operands ':'
	  asm_operands ':' asm_clobbers ')' ';'
			| GOTO '*' expr ';'
			| GOTO identifier ';'
			| label_colon stmt
			| label_colon '}'
		'}'	| ';'
			| try_block
	;

function_try_block:
	  TRY
			  ctor_initializer_opt compstmt
			  handler_seq
			;

try_block:
	  TRY
			  compstmt
			  handler_seq
			;

handler_seq:
	  handler
	| handler_seq handler
	;

handler:
	  CATCH
			  .pushlevel handler_args
			  
	  compstmt
			  
	  .poplevel
	;

type_specifier_seq:
	  typed_typespecs  %prec EMPTY
	| nonempty_cv_qualifiers  %prec EMPTY
	;

handler_args:
	  '(' ELLIPSIS ')'
			/* This doesn't allow reference parameters, the below does.
	| '(' type_specifier_seq absdcl ')'
			| '(' type_specifier_seq ')'
			| '(' type_specifier_seq notype_declarator ')'
			| '(' typed_typespecs after_type_declarator ')'
			This allows reference parameters...  */
	| '(' parm ')'
			;

label_colon:
	  IDENTIFIER ':'
			| PTYPENAME ':'
			| TYPENAME ':'
			| SELFNAME ':'
			;

for.init.statement:
	  xexpr ';'
			| decl
	| '{' compstmtend
			;

/* Either a type-qualifier or nothing.  First thing in an `asm' statement.  */

maybe_cv_qualifier:
	  /* empty */
			| CV_QUALIFIER
			;

xexpr:
	  /* empty */
			| expr
	| error
			;

/* These are the operands other than the first string and colon
   in  asm ("addextend %2,%1": "=dm" (x), "0" (y), "g" (*x))  */
asm_operands:
	  /* empty */
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

parmlist:
	  /* empty */
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

/* A default argument to a */
defarg:
	  '='
			  defarg1
			;

defarg1:
	  DEFARG
	| init
	;

/* A nonempty list of parameter declarations or type names.  */
parms:
	  named_parm
			| parm defarg
			| parms_comma full_parm
			| parms_comma bad_parm
			| parms_comma bad_parm '=' init
			;

parms_comma:
	  parms ','
	| type_id ','
			;

/* A single parameter declaration or parameter type name,
   as found in a parmlist.  */
named_parm:
	/* Here we expand typed_declspecs inline to avoid mis-parsing of
	   TYPESPEC IDENTIFIER.  */
	  typed_declspecs1 declarator
			| typed_typespecs declarator
			| typespec declarator
			| typed_declspecs1 absdcl
			| typed_declspecs1  %prec EMPTY
			| declmods notype_declarator
			;

full_parm:
	  parm
			| parm defarg
			;

parm:
	  named_parm
	| type_id
	;

see_typename:
	  /* empty */  %prec EMPTY
			;

bad_parm:
	  /* empty */ %prec EMPTY
			| notype_declarator
			;

exception_specification_opt:
	  /* empty */  %prec EMPTY
			| THROW '(' ansi_raise_identifiers  ')'  %prec EMPTY
			| THROW LEFT_RIGHT  %prec EMPTY
			;

ansi_raise_identifier:
	  type_id
			;

ansi_raise_identifiers:
	  ansi_raise_identifier
	| ansi_raise_identifiers ',' ansi_raise_identifier
			;

conversion_declarator:
	  /* empty */  %prec EMPTY
			| '*' cv_qualifiers conversion_declarator
			| '&' cv_qualifiers conversion_declarator
			| ptr_to_mem cv_qualifiers conversion_declarator
			;

operator:
	  OPERATOR
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
			| operator NEW  %prec EMPTY
			| operator DELETE  %prec EMPTY
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
