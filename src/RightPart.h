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

#ifndef RIGHTPART_H
#define RIGHTPART_H

/** \file RightPart.h 
 ** Defines the classes in the RightPart hierarchy,
 ** including the abstract base class RightPart, and its derived
 ** classes: TerminalRightPart, NonTerminalRightPart, UnionRightPart,
 ** ConcatRightPart, KleeneClosureRightPart, PositiveClosureRightPart,
 ** OptionRightPart and EmptyRightPart
 **/
#include <iostream>
using namespace std;

#include "Visitor.h"

/** This class represents the rightpart (ie right-hand-side) of a
 ** production rule.  It it the abstract base class for a family of
 ** classes that represent the RHS as an abstract syntax tree.
 **/
class RightPart
{
 public:
  /** All RightParts accept a Visitor, which simply calls the
   ** specialised visitXXX method for each of the derived classes of
   ** RightPart.
   **/
  virtual void accept(Visitor *) = 0;
  friend ostream & operator<<(ostream & out, const RightPart & rp);
  virtual void printMe(ostream &) const = 0;
};

/** This class represents the occurrence of a terminal symbol on the
 ** RHS of a rule. 
 **/
class TerminalRightPart : public RightPart
{
 public:
  TerminalRightPart(Terminal * term) : theTerminal(term) {}
  void printMe(ostream & out) const ;
  void accept(Visitor *v) { 
    return v->visitTerminal(theTerminal);
  }
 private:
  Terminal * theTerminal;
};


/** This class represents the occurrence of a non-terminal symbol on the
 ** RHS of a rule. 
 **/
class NonTerminalRightPart : public RightPart
{
 public:
  NonTerminalRightPart(NonTerminal * nonterm) : nonTerminal(nonterm) {}
  void printMe(ostream & out) const ;
  void accept(Visitor *v) { 
    return v->visitNonTerminal(nonTerminal); 
  }
 private:
  NonTerminal * nonTerminal;
};

/** This represents the union operation over two other rightparts. 
 **/
class UnionRightPart : public RightPart
{
 public:
  UnionRightPart(RightPart * left, RightPart * right)
    : rp1(left), rp2(right) {}
  void printMe(ostream & out) const ;
  void accept(Visitor *v) { 
    return v->visitUnion(rp1,rp2); 
  }
 private:
  RightPart *rp1, *rp2;  /** union: either rp1 or rp2 */
}; 

/** This represents the concatenation operation over two other rightparts. 
 **/
class ConcatRightPart : public RightPart
{
 public:
  ConcatRightPart(RightPart * left, RightPart * right)
    : rp1(left), rp2(right) {}
  void printMe(ostream & out) const ;
  void accept(Visitor *v) { return v->visitConcat(rp1,rp2); }
 private:
  RightPart *rp1, *rp2;  /** concat: rp1 followed by rp2 */
}; 

/** This represents the Kleene closure operation over another rightpart.
 ** i.e. zero or more occurrences of that rightpart.
 **/
class KleeneClosureRightPart : public RightPart
{
 public:
  KleeneClosureRightPart(RightPart * cld) : closed(cld) {}
  void printMe(ostream & out) const ;
  void accept(Visitor *v) { return v->visitKleeneClosure(closed); }
 private:
  RightPart *closed; /** Zero or more of closed */
}; 

/** This represents the positive closure operation over another rightpart.
 ** i.e. one or more occurrences of that rightpart.
 **/
class PositiveClosureRightPart : public RightPart
{
 public:
  PositiveClosureRightPart(RightPart * cld) : closed(cld) {}
  void printMe(ostream & out) const ;
  void accept(Visitor *v) { return v->visitPositiveClosure(closed); }
 private:
  RightPart *closed;  /** One or more of closed */
}; 


/** This represents the option operator applied to another rightpart.
 ** i.e. zero or once occurrences of that rightpart.
 **/
class OptionRightPart : public RightPart
{
 public:
  OptionRightPart(RightPart * opt) : option(opt) {}
  void printMe(ostream & out) const ;
  void accept(Visitor *v) { return v->visitOption(option); }
 private:
  RightPart *option;  /** Zero or one of option */
}; 


/** This represents the empty string as it occurs in a rightpart.
 ** This is a singleton class, since all empty strings are the same.
 **/
class EmptyRightPart : public RightPart
{
 public:
  /** Returns the (singleton) instance of EmptyRightPart */
  static EmptyRightPart * Instance()  {
    static EmptyRightPart theInstance;
    return &theInstance;
  }
  void printMe(ostream & out) const ;
  void accept(Visitor *v) { return v->visitEmpty(); }
 private:
  /** This is a singleton class, so the constructor is private */
  EmptyRightPart()  {}
}; 



#endif
