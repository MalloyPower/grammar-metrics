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

#ifndef GRAMMAR_H
#define GRAMMAR_H


/** \file Grammar.h
 ** Defines the class Grammar
 **/

#include <string>
#include <vector>

#include "RightPart.h"
#include "Vocab.h"
#include "GrammarLevels.h"

typedef vector<NonTerminal*>::const_iterator NontIter;
typedef vector<Terminal*>::const_iterator TermIter;

/** This class represents a grammar as a list of production rules.  It
 ** also provides the main methods that collect the metrics (usually
 ** by running the appropriate visitor), and some structures to hold
 ** those values.  This is pretty much the central class in the whole
 ** tool; once the input grammar has been parsed, everything else is
 ** initiated from here and collected in here.
 **/
class Grammar 
{

 public:

  Grammar() : startSymbol(NULL), levels(NULL) {}

  friend ostream & operator<<(std::ostream & out, const Grammar & gram) {
    out << "START SYMBOL: " << *(gram.startSymbol) << std::endl;
    out << "TERMINALS:" << std::endl;
    for(TermIter ptr = gram.terminals.begin();
        ptr != gram.terminals.end(); ++ptr) {
      out << **ptr << "\t";
    }
    out << std::endl;
    out << "NON-TERMINALS:" << std::endl;
    for (NontIter normPtr = gram.prodRules.begin();
         normPtr != gram.prodRules.end(); ++normPtr) {
      (*normPtr)->printDefinition(out);
    }
    return out;
  }


  Terminal *addTerminal(const string &);  
  Terminal *getTerminal(const string &);  
  NonTerminal *addNonTerminal(const string &);  
  NonTerminal *getNonTerminal(const string &);  

  int printUndefined(ostream &);
  int countTerminals() const {
    return terminals.size();
  }
  int countNonTerminals() const {
    return prodRules.size();
  }

  void makeDerivesRelation();
  void printDerivesRelation(ostream &);
  void makeEquivClasses();
  void printEquivClasses(ostream &);
  void printEquivClassSizes(ostream &);
  void printEquivGraph(ostream &out);

  float countNormEquivClasses();
  int countEquivClasses(int=0);
  int getLargestEquivClassSize();

  float averageSize();
  float getOneImpurity();
  float getManyImpurity();

  int calcMcCabe();
  void printMcCabe(ostream &);
  float calcHalstead();
  float calcHeight();

 private:
  void calcDerivesInOne();
  void makeEquivGraph();
  int calcGlobalHeight(int,int);

  /** To store the production rules we just need a list of
   ** non-terminals, since each of these has a pointer to its
   ** definition.
  **/
  vector<NonTerminal*> prodRules;
  /** A list of all the terminal symbols used in the grammar */
  vector<Terminal*> terminals;
  /** A pointer to the start symbol (whihc is also contained in prodRules) */
  NonTerminal * startSymbol;

  /** All the grammatical levels (derives-in-many relationship) */
  GrammarLevels *levels;

  /** A list of the equivalence classes, each of which is a list of
   ** equivalent non-terminals.
  **/
  vector< vector<NonTerminal *> > equivClasses;

  /** The equivalence graph, effectively a NxN matrix of booleans,
   ** representing the irreflexive, anti-symmetric "can derive"
   ** relation between equivalence classes.
  **/
  vector< vector<bool> > equivGraph;

  /** Holds the McCabe value for each non-terminal */
  vector<int> localMcCabe;

  /** Holds the Varju height value for each equlvalence class */
  vector<int> varjuHeight;

};

#endif
