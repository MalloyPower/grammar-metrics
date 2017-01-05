/* Java grammar, version 1.1
 * Based on the "Java Language Specification",
 * by James Gosling and Bill Joy and Guy Steele, Addison-Wesley, 1996.
 */

/********** Keywords **********/
%token  abstract        default if      private throw
%token  boolean do      implements      protected       throws
%token  break   double  import  public  transient
%token  byte    else    instanceof      return  try
%token  case    extends int     short   void
%token  catch   final   interface       static  volatile
%token  char    finally long    super   while
%token  class   float   native  switch
%token  const   for     new     synchronized
%token  continue        goto    package this

/*** Multi-Character Operators ***/
%token PLUSEQ MINUSEQ STAREQ DIVEQ MODEQ
%token XOREQ  ANDEQ   OREQ   LTLT  GTGT 
%token GTGTEQ     LTLTEQ     EQEQ   NOTEQ
%token LEQ    GEQ     ANDAND OROR  PLUSPLUS MINUSMINUS 
%token GTGTGT GTGTGTEQ

/********** Literals **********/
%token IntegerLiteral    FloatingPointLiteral
%token BooleanLiteral    NullLiteral
%token CharacterLiteral  StringLiteral
%token Identifier

%%

/********** 19.2 Productions from section 2.3: The Syntactic Grammar **********/

Goal
  : CompilationUnit
  ;

/********** 19.3 Productions from section 3: Lexical Structure **********/



Literal
  : IntegerLiteral
  | FloatingPointLiteral
  | BooleanLiteral
  | CharacterLiteral
  | StringLiteral
  | NullLiteral
  ;

/********** 19.4 Productions from section 4: Types, Values, and Variables **********/

Type
  : PrimitiveType
  | ReferenceType
  ;

PrimitiveType
  : NumericType
  | boolean
  ;

NumericType
  : IntegralType
  | FloatingPointType
  ;

IntegralType
  : byte | short | int | long | char
  ;

FloatingPointType
  : float | double
  ;

ReferenceType
  : ClassOrInterfaceType
  | ArrayType
  ;

ClassOrInterfaceType
  : ClassType
  | InterfaceType
  ;

ClassType
  : TypeName
  ;

InterfaceType
  : TypeName
  ;

ArrayType
  : Type '[' ']'
  ;

/********** 19.5 Productions from section 6: Names **********/


PackageName
  : Identifier
  | PackageName '.' Identifier
  ;

TypeName
  : Identifier
  | PackageName '.' Identifier
  ;

ExpressionName
  : Identifier
  | AmbiguousName '.' Identifier
  ;

MethodName
  : Identifier
  | AmbiguousName '.' Identifier
  ;

AmbiguousName
  : Identifier
  | AmbiguousName '.' Identifier
  ;

/********** 19.6 Productions from section 7: Packages **********/


CompilationUnit
  : [ PackageDeclaration ] [ ImportDeclarations ] [ TypeDeclarations ]
  ;

ImportDeclarations
  : ImportDeclaration
  | ImportDeclarations ImportDeclaration
  ;

TypeDeclarations
  : TypeDeclaration
  | TypeDeclarations TypeDeclaration
  ;

PackageDeclaration
  : package PackageName ';'
  ;

ImportDeclaration
  : SingleTypeImportDeclaration
  | TypeImportOnDemandDeclaration
  ;

SingleTypeImportDeclaration
  : import TypeName ';'
  ;

TypeImportOnDemandDeclaration
  : import PackageName '.' '*' ';'
  ;

TypeDeclaration
  : ClassDeclaration
  | InterfaceDeclaration
  | ';'
  ;

/********** 19.7 Productions Used Only in the LALR(1) Grammar **********/
/* (omitted) */

/********** 19.8 Productions from section 8: Classes **********/

/***** 19.8.1 Productions from section 8.1: Class Declaration *****/


ClassDeclaration
  : [ ClassModifiers ] class Identifier [ Super ] [ Interfaces ] ClassBody
  ;

ClassModifiers
  : ClassModifier
  | ClassModifiers ClassModifier
  ;

ClassModifier
  : public | abstract | final
  ;

Super
  : extends ClassType
  ;

Interfaces
  : implements InterfaceTypeList
  ;

InterfaceTypeList
  : InterfaceType
  | InterfaceTypeList ',' InterfaceType
  ;

ClassBody
  : '{' [ ClassBodyDeclarations ] '}'
  ;

ClassBodyDeclarations
  : ClassBodyDeclaration
  | ClassBodyDeclarations ClassBodyDeclaration
  ;

ClassBodyDeclaration
  : ClassMemberDeclaration
  | StaticInitializer
  | ConstructorDeclaration
  ;

ClassMemberDeclaration
  : FieldDeclaration
  | MethodDeclaration
  ;

/***** 19.8.2 Productions from section 8.3: Field Declarations *****/


FieldDeclaration
  : [ FieldModifiers ] Type VariableDeclarators ';'
  ;

VariableDeclarators
  : VariableDeclarator
  | VariableDeclarators ',' VariableDeclarator
  ;

VariableDeclarator
  : VariableDeclaratorId
  | VariableDeclaratorId '=' VariableInitializer
  ;

VariableDeclaratorId
  : Identifier
  | VariableDeclaratorId '[' ']'
  ;

VariableInitializer
  : Expression
  | ArrayInitializer
  ;

FieldModifiers
  : FieldModifier
  | FieldModifiers FieldModifier
  ;

FieldModifier
  : public | protected | private
  | final | static | transient | volatile
  ;

/***** 19.8.3 Productions from section 8.4: Method Declarations *****/


MethodDeclaration
  : MethodHeader MethodBody
  ;

MethodHeader
  : [ MethodModifiers ] ResultType MethodDeclarator [ Throws ]
  ;

ResultType
  : Type
  | void
  ;

MethodDeclarator
  : Identifier '(' [ FormalParameterList ] ')'
  | MethodDeclarator '[' ']'
  ;

FormalParameterList
  : FormalParameter
  | FormalParameterList ',' FormalParameter
  ;

FormalParameter
  : Type VariableDeclaratorId
  ;

MethodModifiers
  : MethodModifier
  | MethodModifiers MethodModifier
  ;

MethodModifier
  : public | protected | private
  | abstract | static | final | synchronized|  native
  ;

Throws
  : throws ClassTypeList
  ;

ClassTypeList
  : ClassType
  | ClassTypeList ',' ClassType
  ;

MethodBody
  : Block 
  | ';'
  ;

/***** 19.8.4 Productions from section 8.5: Static Initializers *****/

StaticInitializer
  : static Block
  ;

/***** 19.8.5 Productions from section 8.6: Constructor Declarations *****/

ConstructorDeclaration
  : [ ConstructorModifiers ] ConstructorDeclarator [ Throws ] ConstructorBody
  ;

ConstructorDeclarator
  : Identifier '(' [ FormalParameterList ] ')'
  ;
/* Original had SimpleTypeName above instead of Identifier... */

ConstructorModifiers
  : ConstructorModifier
  | ConstructorModifiers ConstructorModifier
  ;

ConstructorModifier
  : public | protected | private
  ;

ConstructorBody
  : '{' [ ExplicitConstructorInvocation ] [ BlockStatements ] '}'
  ;

ExplicitConstructorInvocation
  : this '(' [ ArgumentList ] ')' ';'
  | super '(' [ ArgumentList ] ')' ';'
  ;

/********** 19.9 Productions from section 9: Interfaces **********/

/***** 19.9.1 Productions from section 9.1: Interface Declarations *****/

InterfaceDeclaration
  : [ InterfaceModifiers ] interface Identifier [ ExtendsInterfaces ] InterfaceBody
  ;

InterfaceModifiers
  : InterfaceModifier
  | InterfaceModifiers InterfaceModifier
  ;

InterfaceModifier
  : public | abstract
  ;

ExtendsInterfaces
  : extends InterfaceType
  | ExtendsInterfaces ',' InterfaceType
  ;

InterfaceBody
  : '{' [ InterfaceMemberDeclarations ] '}'
  ;

InterfaceMemberDeclarations
  : InterfaceMemberDeclaration
  | InterfaceMemberDeclarations InterfaceMemberDeclaration
  ;

InterfaceMemberDeclaration
  : ConstantDeclaration
  | AbstractMethodDeclaration
  ;

ConstantDeclaration
  : ConstantModifiers Type VariableDeclarator
  ;

ConstantModifiers
  : public | static | final
  ;

AbstractMethodDeclaration
  : [ AbstractMethodModifiers ] ResultType MethodDeclarator [ Throws ] ';'
  ;

AbstractMethodModifiers
  : AbstractMethodModifier
  | AbstractMethodModifiers AbstractMethodModifier
  ;

AbstractMethodModifier
  : public | abstract
  ;

/********** 19.10 Productions from section 10: Arrays **********/


ArrayInitializer
  : '{' [ VariableInitializers ] [ ',' ] '}'
  ;

VariableInitializers
  : VariableInitializer
  | VariableInitializers ',' VariableInitializer
  ;

/********** 19.11 Productions from section 14: Blocks and Statements **********/

Block
  : '{' [ BlockStatements ] '}'
  ;

BlockStatements
  : BlockStatement
  | BlockStatements BlockStatement
  ;

BlockStatement
  : LocalVariableDeclarationStatement
  | Statement
  ;

LocalVariableDeclarationStatement
  : LocalVariableDeclaration ';'
  ;

LocalVariableDeclaration
  : Type VariableDeclarators
  ;

Statement
  : StatementWithoutTrailingSubstatement
  | LabeledStatement
  | IfThenStatement
  | IfThenElseStatement
  | WhileStatement
  | ForStatement
  ;

StatementNoShortIf
  : StatementWithoutTrailingSubstatement
  | LabeledStatementNoShortIf
  | IfThenElseStatementNoShortIf
  | WhileStatementNoShortIf
  | ForStatementNoShortIf
  ;

StatementWithoutTrailingSubstatement
  : Block
  | EmptyStatement
  | ExpressionStatement
  | SwitchStatement
  | DoStatement
  | BreakStatement
  | ContinueStatement
  | ReturnStatement
  | SynchronizedStatement
  | ThrowStatement
  | TryStatement
  ;

EmptyStatement
  : ';'
  ;

LabeledStatement
  : Identifier ':' Statement
  ;

LabeledStatementNoShortIf
  : Identifier ':' StatementNoShortIf
  ;

ExpressionStatement
  : StatementExpression ';'
  ;

StatementExpression
  : Assignment
  | PreIncrementExpression
  | PreDecrementExpression
  | PostIncrementExpression
  | PostDecrementExpression
  | MethodInvocation
  | ClassInstanceCreationExpression
  ;

IfThenStatement
  : if '(' Expression ')' Statement
  ;

IfThenElseStatement
  : if '(' Expression ')' StatementNoShortIf else Statement
  ;

IfThenElseStatementNoShortIf
  : if '(' Expression ')' StatementNoShortIf else StatementNoShortIf
  ;

SwitchStatement
  : switch '(' Expression ')' SwitchBlock
  ;

SwitchBlock
  : '{' [ SwitchBlockStatementGroups ] [ SwitchLabels ] '}'
  ;

SwitchBlockStatementGroups
  : SwitchBlockStatementGroup
  | SwitchBlockStatementGroups SwitchBlockStatementGroup
  ;

SwitchBlockStatementGroup
  : SwitchLabels BlockStatements
  ;

SwitchLabels
  : SwitchLabel
  | SwitchLabels SwitchLabel
  ;

SwitchLabel
  : case ConstantExpression ':'
  | default ':'
  ;

WhileStatement
  : while '(' Expression ')' Statement
  ;

WhileStatementNoShortIf
  : while '(' Expression ')' StatementNoShortIf
  ;

DoStatement
  : do Statement while '(' Expression ')' ';'
  ;

ForStatement
  : for '(' [ ForInit ] ';' [ Expression ] ';' [ ForUpdate ] ')'
  | Statement
  ;

ForStatementNoShortIf
  : for '(' [ ForInit ] ';' [ Expression ] ';' [ ForUpdate ] ')'
  | StatementNoShortIf
  ;

ForInit
  : StatementExpressionList
  | LocalVariableDeclaration
  ;

ForUpdate
  : StatementExpressionList
  ;

StatementExpressionList
  : StatementExpression
  | StatementExpressionList ',' StatementExpression
  ;

BreakStatement
  : break [ Identifier ] ';'
  ;

ContinueStatement
  : continue [ Identifier ] ';'
  ;

ReturnStatement
  : return [ Expression ] ';'
  ;

ThrowStatement
  : throw Expression ';'
  ;

SynchronizedStatement
  : synchronized '(' Expression ')' Block
  ;

TryStatement
  : try Block Catches
  | try Block [ Catches ] Finally
  ;

Catches
  : CatchClause
  | Catches CatchClause
  ;

CatchClause
  : catch '(' FormalParameter ')' Block
  ;

Finally
  : finally Block
  ;

/********** 19.12 Productions from section 15: Expressions **********/

Primary
  : PrimaryNoNewArray
  | ArrayCreationExpression
  ;

PrimaryNoNewArray
  : Literal
  | this
  | '(' Expression ')'
  | ClassInstanceCreationExpression
  | FieldAccess
  | MethodInvocation
  | ArrayAccess
  ;

ClassInstanceCreationExpression
  : new ClassType '(' [ ArgumentList ] ')'
  ;

ArgumentList
  : Expression
  | ArgumentList ',' Expression
  ;

ArrayCreationExpression
  : new PrimitiveType DimExprs [ Dims ]
  | new TypeName DimExprs [ Dims ]
  ;

DimExprs
  : DimExpr
  | DimExprs DimExpr
  ;

DimExpr
  : '[' Expression ']'
  ;

Dims
  : '[' ']'
  | Dims '[' ']'
  ;

FieldAccess
  : Primary '.' Identifier
  | super '.' Identifier
  ;

MethodInvocation
  : MethodName '(' [ ArgumentList ] ')'
  | Primary '.' Identifier '(' [ ArgumentList ] ')'
  | super '.' Identifier '(' [ ArgumentList ] ')'
  ;

ArrayAccess
  : ExpressionName '[' Expression ']'
  | PrimaryNoNewArray '[' Expression ']'
  ;

PostfixExpression
  : Primary
  | ExpressionName
  | PostIncrementExpression
  | PostDecrementExpression
  ;

PostIncrementExpression
  : PostfixExpression PLUSPLUS
  ;

PostDecrementExpression
  : PostfixExpression MINUSMINUS
  ;

UnaryExpression
  : PreIncrementExpression
  | PreDecrementExpression
  | '+' UnaryExpression
  | '-' UnaryExpression
  | UnaryExpressionNotPlusMinus
  ;

PreIncrementExpression
  : PLUSPLUS UnaryExpression
  ;

PreDecrementExpression
  : MINUSMINUS UnaryExpression
  ;

UnaryExpressionNotPlusMinus
  : PostfixExpression
  | '~' UnaryExpression
  | '!' UnaryExpression
  | CastExpression
  ;

CastExpression
  : '(' PrimitiveType [ Dims ] ')' UnaryExpression
  | '(' ReferenceType ')' UnaryExpressionNotPlusMinus
  ;

MultiplicativeExpression
  : UnaryExpression
  | MultiplicativeExpression '*' UnaryExpression
  | MultiplicativeExpression '/' UnaryExpression
  | MultiplicativeExpression '%' UnaryExpression
  ;

AdditiveExpression
  : MultiplicativeExpression
  | AdditiveExpression '+' MultiplicativeExpression
  | AdditiveExpression '-' MultiplicativeExpression
  ;

ShiftExpression
  : AdditiveExpression
  | ShiftExpression LTLT AdditiveExpression
  | ShiftExpression GTGT AdditiveExpression
  | ShiftExpression GTGTGT AdditiveExpression
  ;

RelationalExpression
  : ShiftExpression
  | RelationalExpression '<' ShiftExpression
  | RelationalExpression '>' ShiftExpression
  | RelationalExpression LEQ ShiftExpression
  | RelationalExpression GEQ ShiftExpression
  | RelationalExpression instanceof ReferenceType
  ;

EqualityExpression
  : RelationalExpression
  | EqualityExpression EQEQ RelationalExpression
  | EqualityExpression NOTEQ RelationalExpression
  ;

AndExpression
  : EqualityExpression
  | AndExpression '&' EqualityExpression
  ;

ExclusiveOrExpression
  : AndExpression
  | ExclusiveOrExpression '^' AndExpression
  ;

InclusiveOrExpression
  : ExclusiveOrExpression
  | InclusiveOrExpression '|' ExclusiveOrExpression
  ;

ConditionalAndExpression
  : InclusiveOrExpression
  | ConditionalAndExpression ANDAND InclusiveOrExpression
  ;

ConditionalOrExpression
  : ConditionalAndExpression
  | ConditionalOrExpression OROR ConditionalAndExpression
  ;

ConditionalExpression
  : ConditionalOrExpression
  | ConditionalOrExpression '?' Expression ':' ConditionalExpression
  ;

AssignmentExpression
  : ConditionalExpression
  | Assignment
  ;

Assignment
  : LeftHandSide AssignmentOperator AssignmentExpression
  ;

LeftHandSide
  : ExpressionName
  | FieldAccess
  | ArrayAccess
  ;

AssignmentOperator
  : '=' | STAREQ | DIVEQ | MODEQ | PLUSEQ | MINUSEQ 
  | LTLTEQ | GTGTEQ | GTGTGTEQ | XOREQ | ANDEQ | OREQ  
  ;

Expression
  : AssignmentExpression
  ;

ConstantExpression
  : Expression  
  ;


%%
