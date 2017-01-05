/*
/* Grammar for Python, v 2.2
 * Taken from the file Python-2.2/Grammar/Grammar of the Python distribution
 */

%token NAME
%token NUMBER
%token STRING
%token NEWLINE
%token ENDMARKER
%token INDENT
%token DEDENT

%%

Start_symbols 
  : single_input  /* is a single interactive statement; */
  | file_input /* is a module or sequence of commands read from input file */
  | eval_input /* is the input for the eval() and input() functions. */
  ;
single_input: NEWLINE | simple_stmt | compound_stmt NEWLINE ;
file_input: {NEWLINE | stmt} ENDMARKER ;
eval_input: testlist {NEWLINE} ENDMARKER ;
 
funcdef: "def" NAME parameters ":" suite ;
parameters: "(" [varargslist] ")" ;
varargslist: {fpdef ["=" test] ","} ("*" NAME ["," "**" NAME] | "**" NAME) | fpdef ["=" test] {"," fpdef ["=" test]} [","] ;
fpdef: NAME | "(" fplist ")" ;
fplist: fpdef {"," fpdef} [","] ;
 
stmt: simple_stmt | compound_stmt ;
simple_stmt: small_stmt {";" small_stmt} [";"] NEWLINE ;
small_stmt: expr_stmt | print_stmt  | del_stmt | pass_stmt | flow_stmt | import_stmt | global_stmt | exec_stmt | assert_stmt ;
expr_stmt: testlist {augassign testlist | ("=" testlist)} ;
augassign: "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" | "**=" | "//=" ;
/* For normal assignments,  
 *additional restrictions enforced by the interpreter 
 */ 
print_stmt: "print" ( [ test {"," test} [","] ] | ">>" test [ ("," test){"," test} [","] ] ) ;
del_stmt: "del" exprlist ;
pass_stmt: "pass" ;
flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt ;
break_stmt: "break" ;
continue_stmt: "continue" ;
return_stmt: "return" [testlist] ;
yield_stmt: "yield" testlist ;
raise_stmt: "raise" [test ["," test ["," test]]] ;
import_stmt: "import" dotted_as_name {"," dotted_as_name} | "from" dotted_name "import" ("*" | import_as_name {"," import_as_name}) ;
import_as_name: NAME [NAME NAME] ;
dotted_as_name: dotted_name [NAME NAME] ;
dotted_name: NAME {"." NAME} ;
global_stmt: "global" NAME {"," NAME} ;
exec_stmt: "exec" expr ["in" test ["," test]] ;
assert_stmt: "assert" test ["," test] ;

compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | funcdef | classdef ;
if_stmt: "if" test ":" suite {"elif" test ":" suite} ["else" ":" suite] ;
while_stmt: "while" test ":" suite ["else" ":" suite] ;
for_stmt: "for" exprlist "in" testlist ":" suite ["else" ":" suite] ;
try_stmt: ("try" ":" suite ((except_clause ":" suite){except_clause ":" suite}) 
           ["else" ":" suite] | "try" ":" suite "finally" ":" suite) ;
/* NB compile.c makes sure that the default except clause is last */ 
except_clause: "except" [test ["," test]] ;
suite: simple_stmt | NEWLINE INDENT stmt {stmt} DEDENT ;

test: and_test {"or" and_test} | lambdef ;
and_test: not_test {"and" not_test} ;
not_test: "not" not_test | comparison ;
comparison: expr {comp_op expr} ;
comp_op: "<"|">"|"=="|">="|"<="|"<>"|"!="|"in"|"not" "in"|"is"|"is" "not" ;
expr: xor_expr {"|" xor_expr} ;
xor_expr: and_expr {"^" and_expr} ;
and_expr: shift_expr {"&" shift_expr} ;
shift_expr: arith_expr {("<<"|">>") arith_expr} ;
arith_expr: term {("+"|"-") term} ;
term: factor {("*"|"/"|"%"|"//") factor} ;
factor: ("+"|"-"|"~") factor | power ;
power: {atom trailer} {"**" factor} ;
atom: "(" [testlist] ")" | "[" [listmaker] "]" | "{" [dictmaker] "}" | 
"`" testlist "`" | NAME | NUMBER | (STRING {STRING}) ;
listmaker: test ( list_for | {"," test} [","] ) ;
lambdef: "lambda" [varargslist] ":" test ;
trailer: "(" [arglist] ")" | "[" subscriptlist "]" | "." NAME ;
subscriptlist: subscript {"," subscript} [","] ;
subscript: "." "." "." | test | [test] ":" [test] [sliceop] ;
sliceop: ":" [test] ;
exprlist: expr {"," expr} [","] ;
testlist: test {"," test} [","] ;
testlist_safe: test [("," test){"," test} [","]] ;
dictmaker: test ":" test {"," test ":" test} [","] ;

classdef: "class" NAME ["(" testlist ")"] ":" suite ;

arglist: {argument ","} (argument [","]| "*" test ["," "**" test] | "**" test) ;
argument: [test "="] test	/* Really [keyword "="] test */ ;

list_iter: list_for | list_if ;
list_for: "for" exprlist "in" testlist_safe [list_iter] ;
list_if: "if" test [list_iter] ;

