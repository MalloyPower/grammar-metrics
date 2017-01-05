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

#include "Visitor.h"
#include "Vocab.h"
#include "RightPart.h"

/** \file Vocab.cpp 
 ** Defines some simple methods from the NonTerminal class.
 **/

ostream & operator<<(ostream & out, const VocabSymbol & symb) {
  out << symb.name;
  if (!symb.wasUsed) out << "\tNot used";
  return out;
}


ostream & NonTerminal::printDefinition(ostream & out) {
  out << index << ": " << name;
  if (!wasDefined) out << "\tNot defined";
  else out << "\t-> " << *rhs;
  out << endl;
  return out;
}

/** A non-terminal accepts a visitor by simply passing that visitor to
 ** its right-hand-side 
**/
void NonTerminal::accept(Visitor *v)
{
  assert(rhs != NULL);
  rhs->accept(v);
}
