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

#include <iostream>
using namespace std;

#include "Visitor.h"
#include "Vocab.h"
#include "RightPart.h"

/** \file RightPart.cpp
 ** Defines print methods for the derived classes in the RightPart hierarchy.
 **/

/** Implemented to simply call the (over-ridden) printMe() method.
 **/
ostream & 
operator<<(ostream & out, const RightPart & rp) {
  rp.printMe(out);
  return out;
}


void
TerminalRightPart::printMe(ostream & out) const {
  out << *theTerminal;
}


void
NonTerminalRightPart::printMe(ostream & out) const {
  out << *nonTerminal;
}


void
UnionRightPart::printMe(ostream & out) const {
  out << *rp1 << " | " << *rp2;
}
 

void
ConcatRightPart::printMe(ostream & out) const {
  out << *rp1 << " " << *rp2;
}

void
KleeneClosureRightPart::printMe(ostream & out) const {
  out << "{" << *closed << "}";
}

void
PositiveClosureRightPart::printMe(ostream & out) const {
  out << "{" << closed << "}+";
}


void
OptionRightPart::printMe(ostream & out) const {
  out << "[" << *option << "]";
}

void
EmptyRightPart::printMe(ostream & out) const {
  out << "/* Empty */" ;
}
