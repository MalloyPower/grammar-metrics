/***
 *** SynQ: SYNtactic Quantification of grammar complexity.
 *** Copyright 2004 James F. Power, NUI Maynooth, Ireland <jpower@cs.nuim.ie>
 *** This version: 16 August 2004
 ***
 *** This program is free software; you can redistribute it and/or modify
 *** it under the terms of the GNU General Public License as published by
 *** the Free Software Foundation; either version 2 of the License, or
 *** (at your option) any later version.
 ***
 *** This program is distributed in the hope that it will be useful,
 *** but WITHOUT ANY WARRANTY; without even the implied warranty of
 *** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *** GNU General Public License for more details.
 ***
 *** You should have received a copy of the GNU General Public License
 *** along with this program; if not, write to the Free Software
 *** Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 ***/

/** \file ebnf.y 
 **
 ** This is the bison source for a parser that reads grammar
 ** definitions.  We assume the input is in bison/yacc format, and try
 ** to parse grammar defintitions, basically building up RightPart
 ** objects, and then adding these as the definition of the
 ** appropriate non-terminal.
 **
 ** The scanner has laready added the terminals and non-terminals to
 ** the grammar, so there is relatively little work to do here.
 **
 ** We actually allow some EBNF constructs here, with [] for option,
 ** and {} for repeats (Kleene closure).
**/
%{
   #include <iostream>
   #include "Grammar.h"
   EmptyRightPart * theEmptyRightPart = EmptyRightPart::Instance();
   int yylex();
   void yyerror(const char* s);
%}



%union {
  RightPart *rp;
  Terminal *term;
  NonTerminal *nonterm;
}

%token <term> TERMINAL 
%token <nonterm> NONTERMINAL
%type <rp> rightpart concatrightpart simplerightpart

%left BAR
%token COLON SEMIC  
%token LSQUARE RSQUARE LCURLY RCURLY LPAREN RPAREN


%%
grammar 
  : grammar rule
  | rule
  ;

rule
  : NONTERMINAL COLON rightpart SEMIC  { 
      if ($1->isDefined()) yyerror("Can't re-define a non-terminal");
      else $1->setRightPart($3); 
    }
  ;


rightpart
  : rightpart BAR rightpart     { $$=new UnionRightPart($1,$3); }
  | concatrightpart             { $$=$1; }
  | /* Empty */                 { $$=theEmptyRightPart; }
  ;

concatrightpart
  : concatrightpart simplerightpart  { $$=new ConcatRightPart($1,$2); }
  | simplerightpart                  { $$=$1; }
  ;


simplerightpart
  : TERMINAL                     { $1->makeUsed(); $$=new TerminalRightPart($1); }
  | NONTERMINAL                  { $1->makeUsed(); $$=new NonTerminalRightPart($1); }
  | LSQUARE rightpart RSQUARE    { $$=new OptionRightPart($2); }
  | LCURLY rightpart RCURLY      { $$=new KleeneClosureRightPart($2); }
  | LPAREN rightpart RPAREN      { $$=$2; }
  ;


%%

/** The current input lexeme; maintained by the scanner */
extern char *yytext;
/** The current input line number; maintained by the scanner */
extern int yylineno;

/** Report a syntax error in the input grammar file */
void yyerror(const char * s) 
{ 
  std::cout <<  s << " at line " << yylineno
            << " with " << yytext << std::endl; 
}
