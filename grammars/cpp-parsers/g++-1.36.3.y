/* YACC parser for C++ syntax.
   Copyright (C) 1988, 1989 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@mcc.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This grammar is based on the GNU CC grammar.  */

/* Also note: this version contains experimental exception
   handling features.  They could break, change, disappear,
   or otherwise exhibit volatile behavior.  Don't depend on
   me (Michael Tiemann) to protect you from any negative impact
   this may have on your professional, personal, or spiritual life.  */


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
%token SIZEOF ENUM  IF ELSE WHILE DO FOR SWITCH CASE DEFAULT
%token BREAK CONTINUE RETURN GOTO ASM TYPEOF ALIGNOF
%token ATTRIBUTE

/* the reserved words... C++ extensions */
%token <ttype> AGGR
%token DELETE NEW OVERLOAD PRIVATE PUBLIC PROTECTED THIS OPERATOR
%token DYNAMIC POINTSAT_LEFT_RIGHT LEFT_RIGHT
%token <itype> SCOPE

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
%left <code> ARITHCOMPARE
%left <code> LSHIFT RSHIFT
%left <code> '+' '-'
%left <code> '*' '/' '%'
%right <code> UNARY PLUSPLUS MINUSMINUS
%left HYPERUNARY
%left <ttype> PAREN_STAR_PAREN PAREN_X_SCOPE_STAR_PAREN PAREN_X_SCOPE_REF_PAREN LEFT_RIGHT
%left <code> POINTSAT '.' '(' '['

%right SCOPE			/* C++ extension */
%nonassoc NEW DELETE RAISE RAISES RERAISE TRY EXCEPT CATCH
%right DYNAMIC

%type <code> unop

%type <ttype> identifier IDENTIFIER TYPENAME CONSTANT expr nonnull_exprlist exprlist
%type <ttype> expr_no_commas cast_expr unary_expr primary string STRING
%type <ttype> typed_declspecs reserved_declspecs
%type <ttype> typed_typespecs reserved_typespecquals
%type <ttype> declmods typespec typespecqual_reserved
%type <ttype> SCSPEC TYPESPEC TYPE_QUAL nonempty_type_quals maybe_type_qual
%type <itype> initdecls notype_initdecls initdcl	/* C++ modification */
%type <ttype> init initlist maybeasm
%type <ttype> asm_operands nonnull_asm_operands asm_operand asm_clobbers
%type <ttype> maybe_attribute attribute_list attrib

%type <ttype> compstmt except_stmts

%type <ttype> declarator notype_declarator after_type_declarator

%type <ttype> structsp opt.component_decl_list component_decl_list component_decl components component_declarator
%type <ttype> enumlist enumerator
%type <ttype> typename absdcl absdcl1 type_quals
%type <ttype> xexpr see_typename parmlist parms parm bad_parm

/* C++ extensions */
%token <ttype> TYPENAME_COLON TYPENAME_SCOPE TYPENAME_ELLIPSIS
%token <ttype> PRE_PARSED_FUNCTION_DECL EXTERN_LANG_STRING ALL
%type <ttype> fn.def2 dummy_decl x_typespec return_id
%type <ttype> class_head opt.init base_class_list base_class_visibility_list
%type <ttype> after_type_declarator_no_typename
%type <ttype> maybe_raises raise_identifier raise_identifiers try
%type <ttype> component_declarator0 scoped_identifier
%type <ttype> forhead.1 identifier_or_opname operator_name
%type <ttype> new delete object primary_no_id aggr nonmomentary_expr
%type <itype> LC forhead.2 initdcl0 notype_initdcl0 wrapper member_init_list

%token error


%%
program: /* empty */
	| extdefs
			;

/* the reason for the strange actions in this rule
 is so that notype_initdecls when reached via datadef
 can find a valid list of type and sc specs in $0. */

extdefs:
	   extdef
	| extdefs  extdef
	;

extdef:
	  fndef
			| datadef
			| overloaddef
	| ASM '(' string ')' ';'
			| extern_lang_string '{' extdefs '}'
			| extern_lang_string '{' '}'
			| extern_lang_string fndef
			| extern_lang_string datadef
			;

extern_lang_string:
	  EXTERN_LANG_STRING
			;

overloaddef:
	  OVERLOAD ov_identifiers ';'
	  ;
ov_identifiers: IDENTIFIER
			| ov_identifiers ',' IDENTIFIER
			;
	  
dummy_decl: /* empty */
			;

datadef:
	  dummy_decl notype_initdecls ';'
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
			| dummy_decl notype_declarator error
			;

fn.def1:
	  typed_declspecs declarator maybe_raises
			| declmods notype_declarator maybe_raises
			| dummy_decl notype_declarator maybe_raises
			| dummy_decl TYPENAME '(' parmlist ')' type_quals maybe_raises
			| dummy_decl TYPENAME LEFT_RIGHT type_quals maybe_raises
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
			| dummy_decl notype_declarator maybe_raises
			;

return_id: RETURN IDENTIFIER
			;

return_init: return_id opt.init
			| return_id '(' exprlist ')'
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



member_init: '(' exprlist ')'
			| LEFT_RIGHT
			| identifier '(' exprlist ')'
			| identifier LEFT_RIGHT
			| scoped_identifier identifier '(' exprlist ')'
			| scoped_identifier identifier LEFT_RIGHT
			;

identifier:
	  IDENTIFIER
	| TYPENAME
	;

identifier_or_opname:
	  IDENTIFIER
	| TYPENAME
	| '~' identifier
			| operator_name
			| wrapper IDENTIFIER
			| wrapper TYPENAME
			| wrapper operator_name
			| wrapper scoped_identifier IDENTIFIER
			| wrapper scoped_identifier operator_name
			;

wrapper:  LEFT_RIGHT
			| '~' LEFT_RIGHT
			| LEFT_RIGHT '?'
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

exprlist:
	  /* empty */
			| nonnull_exprlist
	;

nonnull_exprlist:
	  expr_no_commas
			| nonnull_exprlist ',' expr_no_commas
			| nonnull_exprlist ',' error
			;

unary_expr:
	  primary %prec UNARY
			| '*' cast_expr   %prec UNARY
			| '&' cast_expr   %prec UNARY
			| '~' cast_expr   %prec UNARY
			| unop cast_expr  %prec UNARY
			| SIZEOF unary_expr  %prec UNARY
			| SIZEOF '(' typename ')'  %prec HYPERUNARY
			| ALIGNOF unary_expr  %prec UNARY
			| ALIGNOF '(' typename ')'  %prec HYPERUNARY
		
	| new typename %prec '='
			| new x_typespec '(' exprlist ')'
			| new x_typespec LEFT_RIGHT
			| new typename '=' init %prec '='
			| new '(' typename ')'
			/* Unswallow a ':' which is probably meant for ?: expression.  */
	| new TYPENAME_COLON
		
	| delete cast_expr  %prec UNARY
			| delete '[' expr ']' cast_expr  %prec UNARY
			;

cast_expr:
	  unary_expr
	| '(' typename ')' expr_no_commas  %prec UNARY
			| '(' typename ')' '{' initlist maybecomma '}'  %prec UNARY
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
		
	/* Handle general members.  */
	| object '*' expr_no_commas   %prec UNARY
			| object '&' expr_no_commas   %prec UNARY
			| object unop expr_no_commas  %prec UNARY
			| object '(' typename ')' expr_no_commas  %prec UNARY
			| object primary_no_id  %prec UNARY
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
			| primary '(' exprlist ')'
			| primary LEFT_RIGHT
			| primary '[' expr ']'
		'{'	| object identifier_or_opname  %prec UNARY
			| object scoped_identifier identifier_or_opname %prec UNARY
			| primary PLUSPLUS
			| primary MINUSMINUS
		
	/* C++ extensions */
	| THIS
			| dummy_decl TYPE_QUAL '(' exprlist ')'
			| x_typespec '(' exprlist ')'
			| x_typespec LEFT_RIGHT
			| SCOPE IDENTIFIER
			| SCOPE operator_name
			| scoped_identifier identifier_or_opname  %prec HYPERUNARY
			| scoped_identifier identifier_or_opname '(' exprlist ')'
			| scoped_identifier identifier_or_opname LEFT_RIGHT
		
	| object identifier_or_opname '(' exprlist ')'
			| object identifier_or_opname LEFT_RIGHT
			| object scoped_identifier identifier_or_opname '(' exprlist ')'
			| object scoped_identifier identifier_or_opname LEFT_RIGHT
			;

primary_no_id:
	  '(' expr ')'
			| '(' error ')'
			| '('
			  compstmt ')'
			| primary_no_id '(' exprlist ')'
			| primary_no_id LEFT_RIGHT
			| primary_no_id '[' expr ']'
			| primary_no_id PLUSPLUS
			| primary_no_id MINUSMINUS
			| SCOPE IDENTIFIER
			| SCOPE operator_name
			;

new:	  NEW
			| NEW '{' nonnull_exprlist '}'
			| NEW DYNAMIC  %prec EMPTY
			| NEW DYNAMIC '(' string ')'
			| SCOPE new
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
	;

/* Declspecs which contain at least one type specifier or typedef name.
   (Just `const' or `volatile' is not enough.)
   A typedef'd name following these is taken as a name to be declared.  */

typed_declspecs:
	  x_typespec
			| declmods typespec
			| x_typespec reserved_declspecs
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
	  dummy_decl TYPE_QUAL
			| dummy_decl SCSPEC
			| declmods TYPE_QUAL
			| declmods SCSPEC
			;


/* Used instead of declspecs where storage classes are not allowed
   (that is, for typenames and structure components).

   C++ can takes storage classes for structure components.
   Don't accept a typedef-name if anything but a modifier precedes it.  */

typed_typespecs:
	  x_typespec  %prec EMPTY
			| nonempty_type_quals typespec
			| x_typespec reserved_typespecquals
			| nonempty_type_quals typespec reserved_typespecquals
			;

reserved_typespecquals:
	  typespecqual_reserved
			| reserved_typespecquals typespecqual_reserved
			;

/* A typespec (but not a type qualifier).
   Once we have seen one of these in a declaration,
   if a typedef name appears then it is being redeclared.  */

typespec: TYPESPEC
	| structsp
	| TYPENAME
	| TYPEOF '(' expr ')'
			| TYPEOF '(' typename ')'
			;

/* A typespec that is a reserved word, or a type qualifier.  */

typespecqual_reserved: TYPESPEC
	| TYPE_QUAL
	| structsp
	;

x_typespec:
	  dummy_decl TYPESPEC
			| dummy_decl structsp
			| dummy_decl TYPENAME
			| dummy_decl TYPEOF '(' expr ')'
			| dummy_decl TYPEOF '(' typename ')'
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
			| ASM '(' string ')'
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
	    | IDENTIFIER '(' identifiers ')'
	    ;

identifiers:
	  IDENTIFIER
			| identifiers ',' IDENTIFIER
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
			;

structsp:
	  ENUM identifier '{'
			  enumlist maybecomma_warn '}'
			| ENUM '{'
			  enumlist maybecomma_warn '}'
			| ENUM identifier
		
	/* C++ extensions, merged with C to avoid shift/reduce conflicts */
	| class_head LC opt.component_decl_list '}'
			| class_head LC opt.component_decl_list '}' ';'
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
			| DYNAMIC AGGR
			| DYNAMIC '(' string ')' AGGR
			| aggr SCSPEC
			| aggr TYPESPEC
			| aggr TYPE_QUAL
			| aggr AGGR
    ;		
class_head:
	  aggr  %prec EMPTY
			| aggr identifier  %prec EMPTY
			| aggr IDENTIFIER ':' base_class_list  %prec EMPTY
			| aggr TYPENAME_COLON  %prec EMPTY
			| aggr TYPENAME_COLON base_class_list  %prec EMPTY
			;

base_class_list:
	  identifier
			| base_class_visibility_list identifier
			| base_class_list ',' identifier
			| base_class_list ',' base_class_visibility_list identifier
			;

base_class_visibility_list:
	  PUBLIC
			| PRIVATE
			| SCSPEC
			| base_class_visibility_list PUBLIC
			| base_class_visibility_list PRIVATE
			| base_class_visibility_list SCSPEC
			;

LC: '{'
  ;		
opt.component_decl_list:
	/* empty */
			| component_decl_list
			| opt.component_decl_list PUBLIC ':' component_decl_list
			| opt.component_decl_list PRIVATE ':' component_decl_list
			| opt.component_decl_list PROTECTED ':' component_decl_list
			| opt.component_decl_list PUBLIC ':'
	| opt.component_decl_list PRIVATE ':'
	| opt.component_decl_list PROTECTED ':'
	;

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
			| dummy_decl notype_declarator maybe_raises ';'
			| dummy_decl notype_declarator maybe_raises '}'
		'}'	;

components:
	  /* empty: possibly anonymous */
			| component_declarator0
	| components ',' component_declarator
			;

component_declarator0:
	  declarator maybe_raises maybeasm opt.init
			| IDENTIFIER ':' expr_no_commas
			| TYPENAME_COLON expr_no_commas
			| ':' expr_no_commas
			;

component_declarator:
	  declarator maybe_raises maybeasm opt.init
			| IDENTIFIER ':' expr_no_commas
			| TYPENAME_COLON expr_no_commas
			| ':' expr_no_commas
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

typename:
	  typed_typespecs absdcl
			| nonempty_type_quals absdcl
			;

absdcl:   /* an abstract declarator */
	/* empty */ %prec EMPTY
			| absdcl1  %prec EMPTY
	;

nonempty_type_quals:
	  dummy_decl TYPE_QUAL
			| nonempty_type_quals TYPE_QUAL
			;

type_quals:
	  /* empty */
			| type_quals TYPE_QUAL
			;

/* These rules must follow the rules for function declarations
   and component declarations.  That way, longer rules are prefered.  */

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
			| '(' dummy_decl after_type_declarator_no_typename ')'
			| '(' '*' type_quals after_type_declarator ')'
			| PAREN_STAR_PAREN
			| PAREN_X_SCOPE_STAR_PAREN
			| PAREN_X_SCOPE_REF_PAREN
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
			| '(' dummy_decl after_type_declarator_no_typename ')'
			| PAREN_STAR_PAREN
			| PAREN_X_SCOPE_STAR_PAREN
			| PAREN_X_SCOPE_REF_PAREN
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
			| LEFT_RIGHT identifier
			| LEFT_RIGHT '?' identifier
			| '~' LEFT_RIGHT identifier
			| TYPENAME_SCOPE type_quals notype_declarator  %prec '('
			| TYPENAME_SCOPE TYPENAME  %prec '('
			| TYPENAME_SCOPE see_typename TYPENAME '(' nonnull_exprlist ')' type_quals  %prec '.'
			| TYPENAME_SCOPE see_typename TYPENAME '(' parmlist ')' type_quals  %prec '.'
			| TYPENAME_SCOPE see_typename TYPENAME LEFT_RIGHT type_quals  %prec '.'
			| TYPENAME_SCOPE see_typename TYPENAME '(' error ')' type_quals  %prec '.'
			| SCOPE see_typename notype_declarator
			;

scoped_identifier:
	  TYPENAME_SCOPE
	| IDENTIFIER SCOPE
	| scoped_identifier TYPENAME_SCOPE
			;

absdcl1:  /* a nonempty abstract declarator */
	  '(' absdcl1 ')'
			  /* `(typedef)1' is `int'.  */
	| '*' type_quals absdcl1  %prec EMPTY
			| '*' type_quals  %prec EMPTY
			| PAREN_STAR_PAREN
			| PAREN_X_SCOPE_STAR_PAREN
			| PAREN_X_SCOPE_REF_PAREN
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
			| TYPENAME_SCOPE type_quals absdcl1  %prec EMPTY
			| IDENTIFIER SCOPE type_quals absdcl1  %prec EMPTY
			| TYPENAME_SCOPE type_quals %prec EMPTY
			| IDENTIFIER SCOPE type_quals %prec EMPTY
			;

/* at least one statement, the first of which parses without error.  */
/* stmts is used only after decls, so an invalid first statement
   is actually regarded as an invalid decl and part of the decls.  */

stmts:
	  stmt
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

/* This is the body of a function definition.
   It causes syntax errors to ignore to the next openbrace.  */
compstmt_or_error:
	  compstmt
			| error compstmt
	;

compstmt: '{' '}'
			| '{' .pushlevel stmts '}'
			| '{' .pushlevel error '}'
			;

simple_if:
	  IF '(' expr ')'
			  stmt
			;

stmt:
	  compstmt
			| decl
			| expr ';'
			| simple_if ELSE
			  stmt
			| simple_if %prec IF
			| WHILE
			  '(' expr ')'
			  stmt
			| DO
			  stmt WHILE
			  '(' expr ')' ';'
			| forhead.1
			  xexpr ';'
			  xexpr ')'
		/* Don't let the tree nodes for $6 be discarded
		   by clear_momentary during the parsing of the next stmt.  */
			  stmt
			| forhead.2
			  xexpr ';'
			  xexpr ')'
		/* Don't let the tree nodes for $6 be discarded
		   by clear_momentary during the parsing of the next stmt.  */
			  stmt
			| SWITCH '(' expr ')'
			  stmt
			| CASE expr ':'
			  stmt
	| CASE expr RANGE expr ':'
			  stmt
	| DEFAULT ':'
			  stmt
	| BREAK ';'
			| CONTINUE ';'
			| RETURN ';'
			| RETURN expr ';'
			| ASM maybe_type_qual '(' string ')' ';'
			/* This is the case with just output operands.  */
	| ASM maybe_type_qual '(' string ':' asm_operands ')' ';'
			/* This is the case with input operands as well.  */
	| ASM maybe_type_qual '(' string ':' asm_operands ':' asm_operands ')' ';'
			/* This is the case with clobbered registers as well.  */
	| ASM maybe_type_qual '(' string ':' asm_operands ':'
  	  asm_operands ':' asm_clobbers ')' ';'
			| GOTO identifier ';'
			| IDENTIFIER ':'
			  stmt
			| TYPENAME_COLON
			  stmt
			| ';'
		
	/* Exception handling extentions.  */
	| RAISE raise_identifier '(' exprlist ')' ';'
			| RAISE raise_identifier LEFT_RIGHT ';'
			| try EXCEPT identifier '{'
			  except_stmts '}'
			| try RERAISE raise_identifiers /* ';' checked for at bottom.  */
			| try  %prec EMPTY
			;

try:	  TRY '{' '}'
			| try_head stmts '}'
			| try_head error '}'
			;

try_head: TRY '{'  .pushlevel
        ;
except_stmts:
	  /* empty */
			| except_stmts raise_identifier
			  compstmt
			| except_stmts DEFAULT
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
	  parm opt.init
			| parms ',' parm opt.init
			| parms ',' bad_parm opt.init
			;

/* A single parameter declaration or parameter type name,
   as found in a parmlist.  */
parm:
	  typed_declspecs dont_see_typename notype_declarator
			| typed_declspecs dont_see_typename absdcl
			| declmods dont_see_typename notype_declarator
			| declmods dont_see_typename absdcl
			;

see_typename: type_quals
		;

dont_see_typename: /* empty */
		;

bad_parm:
	  dummy_decl notype_declarator
			| dummy_decl absdcl
			;

	/* C++ extension: allow for initialization */
opt.init:
	  /* empty */
			| '=' init
			;

maybe_raises:
	  /* empty */
			| RAISES raise_identifiers  %prec EMPTY
			;

raise_identifier:
	  ALL
			| IDENTIFIER
			| TYPENAME
			| SCOPE IDENTIFIER
			| SCOPE TYPENAME
			| scoped_identifier IDENTIFIER
			| scoped_identifier TYPENAME
	  ;		
raise_identifiers:
	  raise_identifier
	| raise_identifiers ',' raise_identifier
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
			| OPERATOR ARITHCOMPARE
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
			| OPERATOR POINTSAT_LEFT_RIGHT type_quals  %prec '.'
			| OPERATOR LEFT_RIGHT
			| OPERATOR '[' ']'
			| OPERATOR NEW
			| OPERATOR DELETE
		
	/* These should do `groktypename' and set up TREE_HAS_X_CONVERSION
	   here, rather than doing it in class.c .  */
	| OPERATOR typed_typespecs absdcl
			| OPERATOR error
			;



%%

