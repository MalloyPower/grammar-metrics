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

#ifndef VOCABSYMBOL_H
#define VOCABSYMBOL_H

/** \file Vocab.h 
 ** Defines the VocabSymbol class and its derived
 ** classes Terminal and NonTerminal.
 **/


#include <iostream>
#include <string>
#include <assert.h>
using namespace std;

class RightPart;

/** A Vocabulary symbol is either a terminal or a non-terminal.
 ** This is the abstract base class for both of these classes.
 **/
class VocabSymbol {
public:
  friend ostream & operator<<(ostream & out, const VocabSymbol & symb);
  /** Symbols are compared based on their name */
  bool operator!=(const string & symbName) { 
    return name != symbName;
  }
  /** Mark a symbol as having been used in the grammar **/
  void makeUsed() { wasUsed=true; }
  string getName() { return name; }
protected:
  VocabSymbol(const string & n) : name(n), wasUsed(false) {}
  string name;
  bool wasUsed;
 private:
  VocabSymbol() {}

public:
  /** A simple utility class to test for equality between symbols.
   ** This allows us to use the find() method on containers. 
   **/
  struct IsEqual {
    IsEqual(const string & n) : name(n) {}
    bool operator() ( VocabSymbol * right ) const {
      return (right->getName() == name);
    }
    string name;
  };
};

/** This simple class represents terminal symbols.
 **/
class Terminal : public VocabSymbol {
public:
  Terminal(const string & n) : VocabSymbol(n) {}
};



/** This class represents non-terminal symbols.
 ** Each non-terminal contains a pointer to its RHS (right-hand-side),
 ** i.e. its definition. 
 **/
class NonTerminal : public VocabSymbol {
 public:
  NonTerminal(const string & n, int i) 
    : VocabSymbol(n), rhs(NULL), wasDefined(false), index(i) {}
  ostream & printDefinition(ostream & out);
  /** Set the RHS of this non-terminal.  
   ** Note that the non-t takes ownership of the RHS object.
   **/
  void setRightPart(RightPart * right) { 
    assert(right != NULL);
    rhs = right; 
    wasDefined=true;
  }
  bool isDefined() { return wasDefined; };
  void accept(Visitor *v);
  int getIndex() { return index; }

 private:
  RightPart * rhs; /** The right-hand-side (i.e. definition) of this non-t */
  bool wasDefined; /** Whether or not this non-t has been defined */
  int index; /** Can assign an index number to a non-t */

};



#endif
