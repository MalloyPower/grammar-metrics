/* Oberon grammar
 * Based on "The Programming Language Oberon-2" 
 *  by H. Mossenbock and N. Wirth, 
 *  in Structured Programming, 12:4, 1991 pp 179-195.
 * http://www-vs.informatik.uni-ulm.de:81/projekte/Oberon-2.Report/
 */


/********** Keywords **********/
%token ARRAY     IMPORT     RETURN
%token BEGIN     IN         THEN
%token BY        IS         TO
%token CASE      LOOP       TYPE
%token CONST     MOD        UNTIL
%token DIV       MODULE     VAR
%token DO        NIL        WHILE
%token ELSE      OF         WITH
%token ELSIF     OR
%token END       POINTER
%token EXIT      PROCEDURE
%token FOR       RECORD
%token IF        REPEAT

/*** Multi-Character Operators ***/
%token LTEQ GTEQ DOTDOT ASSIGN

/********** Literals **********/
%token number character string ident

%%

Module       : MODULE ident ';' [ImportList] DeclSeq [BEGIN StatementSeq] 
               END ident '.';
ImportList   : IMPORT [ident ASSIGN] ident {',' [ident ASSIGN] ident} ';';
DeclSeq      : { CONST {ConstDecl ';' } | TYPE {TypeDecl ';'} | VAR {VarDecl ';'}} 
               {ProcDecl ';' | ForwardDecl ';'};
ConstDecl    : IdentDef '=' ConstExpr;
TypeDecl     : IdentDef '=' Type;
VarDecl      : IdentList ':' Type;
ProcDecl     : PROCEDURE [Receiver] IdentDef [FormalPars] ';' DeclSeq 
               [BEGIN StatementSeq] END ident;
ForwardDecl  : PROCEDURE '^' [Receiver] IdentDef [FormalPars];
FormalPars   : '(' [FPSection {';' FPSection}] ')' [':' Qualident];
FPSection    : [VAR] ident {',' ident} ':' Type;
Receiver     : '(' [VAR] ident ':' ident ')';
Type         : Qualident
               | ARRAY [ConstExpr {',' ConstExpr}] OF Type 
               | RECORD ['('Qualident')'] FieldList {';' FieldList} END
               | POINTER TO Type
               | PROCEDURE [FormalPars];
FieldList    : [IdentList ':' Type];
StatementSeq : Statement {';' Statement};
Statement    : [ Designator ASSIGN Expr 
               | Designator ['(' [ExprList] ')'] 
               | IF Expr THEN StatementSeq 
                 {ELSIF Expr THEN StatementSeq} 
                 [ELSE StatementSeq] 
                 END 
               | CASE Expr OF Case 
                 {'|' Case} 
                 [ELSE StatementSeq] 
                  END 
               | WHILE Expr DO StatementSeq END 
               | REPEAT StatementSeq UNTIL Expr 
               | FOR ident ASSIGN Expr TO Expr [BY ConstExpr] DO StatementSeq END 
               | LOOP StatementSeq END
               | WITH Guard DO StatementSeq 
                 {'|' Guard DO StatementSeq} 
                 [ELSE StatementSeq] 
                 END
               | EXIT 
               | RETURN [Expr]
               ];
Case         : [CaseLabels {',' CaseLabels} ':' StatementSeq];
CaseLabels   : ConstExpr [DOTDOT ConstExpr];
Guard        : Qualident ':' Qualident;
ConstExpr    : Expr;
Expr         : SimpleExpr [Relation SimpleExpr];
SimpleExpr   : ['+' | '-'] Term {AddOp Term};
Term         : Factor {MulOp Factor};
Factor       : Designator ['(' [ExprList] ')'] | number | character | string 
               | NIL | Set | '(' Expr ')' | '~' Factor;
Set          : '{' [Element {',' Element}] '}';
Element      : Expr [DOTDOT Expr];
Relation     : '=' | '#' | '<' | LTEQ | '>' | GTEQ | IN | IS;
AddOp        : '+' | '-' | OR;
MulOp        : '*' | '/' | DIV | MOD | '&';
Designator   : Qualident {'.' ident | '[' ExprList ']' | '^' | '(' Qualident ')'};
ExprList     : Expr {',' Expr};
IdentList    : IdentDef {',' IdentDef};
Qualident    : [ident '.'] ident;
IdentDef     : ident ['*' | '-'];

%%
