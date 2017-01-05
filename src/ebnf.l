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

/** \file ebnf.l
 ** This is the flex source for a scanner that tokenises grammar
 ** definitions.  We assume the input is in bison/yacc format, and try
 ** to read:
 **  - the token definitions (from %token/left/right/nonassoc directives)
 **  - the start symbol (from the %start directive)
 ** 
 ** Comments and %{ %} user code are filtered out; the rest is passed
 ** to the parser.  Assumes that there are no semantic actions
 ** (i.e. C-code) at the end of the rules.
 **
 ** Terminals and non-terminals are added to the grammar, and the
 ** attribute pased to the parser is a Terminal/NonTerminal object.
 **
 ** We actually allow some EBNF constructs here, with [] for options,
 ** {} for repeats.
**/

%{
#include "Grammar.h"
#include "ebnf.tab.h"
#define YY_MAIN 0

extern void yyerror(const char *);

extern Grammar theGrammar;

int grammarPart = 0;

%}
%option yylineno
%option noyywrap
%option stack

digit       [0-9]
letter      [\.a-zA-Z_]
WS          [ \t\n]
IDENT       {letter}({letter}|{digit})*
L_CHAR      '[^']'
L_STR       \"[^"]+\"
LITRL       {L_CHAR}|{L_STR}
aTYPE       "<"[^>]+">"

%x TOKENS
%x COMMENT
%x USERCODE
%x USERUNION
%x SSYMBOL

%%

"%token"        { BEGIN(TOKENS); }
"%left"         { BEGIN(TOKENS); }
"%right"        { BEGIN(TOKENS); }
"%nonassoc"     { BEGIN(TOKENS); }
<TOKENS>[ \t]   { ; }
<TOKENS>{IDENT} { if (theGrammar.getTerminal(yytext)==NULL) theGrammar.addTerminal(yytext); }
<TOKENS>{LITRL} { if (theGrammar.getTerminal(yytext)==NULL) theGrammar.addTerminal(yytext); }
<TOKENS>{aTYPE} { ; }
<TOKENS>[\n]    { BEGIN(0); }
<TOKENS>"/*"    { yy_push_state(COMMENT); }
<TOKENS>"//".*  { BEGIN(0); }

"%start"        { BEGIN(SSYMBOL); }
<SSYMBOL>{IDENT} { theGrammar.addNonTerminal(yytext); }
<SSYMBOL>"/*"   { yy_push_state(COMMENT); }  
<SSYMBOL>.      { ; }
<SSYMBOL>\n     { BEGIN(0); }

"%type".*       { ; }
"%prec".*       { ; }

"%union"          { BEGIN(USERUNION); }
<USERUNION>"}"    { BEGIN(0); }
<USERUNION>.      { ; }
<USERUNION>\n     { ; }


"//".*          { ; }
"#".*           { ; }
"/*"            { yy_push_state(COMMENT); }
<COMMENT>"*/"   { yy_pop_state(); }
<COMMENT>.      { ; }
<COMMENT>\n     { ; }


"%{"             { BEGIN(USERCODE); }
<USERCODE>"%}"   { BEGIN(0); }
<USERCODE>.      { ; }
<USERCODE>\n     { ; }

"%%"    { grammarPart++; if (grammarPart>1) return(EOF); }

":"     { return COLON; }
";"     { return SEMIC; }
"{"     { return LCURLY; }
"}"     { return RCURLY; }
"["     { return LSQUARE; }
"]"     { return RSQUARE; }
"("     { return LPAREN; }
")"     { return RPAREN; }
"|"     { return BAR; }

{WS}    { ; }

{LITRL} {
          yylval.term = theGrammar.getTerminal(yytext); 
          if (yylval.term == NULL)
            yylval.term = theGrammar.addTerminal(yytext);          
          return TERMINAL;
        }

{IDENT} { 
          yylval.term = theGrammar.getTerminal(yytext); 
          if (yylval.term == NULL)
          {
            yylval.nonterm = theGrammar.getNonTerminal(yytext); 
            if (yylval.nonterm == NULL)
              yylval.nonterm = theGrammar.addNonTerminal(yytext);          
            return NONTERMINAL;
          }
          else // Is a terminal
          {
            return TERMINAL;
          }
        }

.       { yyerror("lexical error"); }


%%

