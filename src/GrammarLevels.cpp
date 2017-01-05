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

#include "RightPart.h"
#include "Vocab.h"
#include "GrammarLevels.h"

/** \file GrammarLevels.cpp 
 ** Defines some of the larger methods in the
 ** GrammarLevels class, mainly to do with calcuating the derives
 ** relationship.
 **/

/** Just print the derives-in-one and derives-in-many tables 
 **/
ostream & 
operator<<(ostream & out, const GrammarLevels & levels) 
{
  out << "DERIVES-IN-ONE RELATION" << endl << levels.derivesInOne;
  out << "DERIVES-IN-MANY RELATION" << endl << levels.derivesInMany;
  return out;
}

/** Set the derives-in-one relationship from nt1 to nt2.
 ** That is, nt2 has been found on the RHS of the rule for nt1.
 **/
void  
GrammarLevels::setDerive(NonTerminal *nt1, NonTerminal *nt2)
{
  derivesInOne.setOrder(nt1->getIndex(),nt2->getIndex());
}

/** Return true if nt1 and nt2 are equivalent.  That is, nt1
 ** can derive nt2 in one or more steps, AND nt2 can derive nt1 in one
 ** or more steps.
 **/
bool 
GrammarLevels::areEquiv(NonTerminal *nt1, NonTerminal *nt2)
{
  return derivesInMany.areEquiv(nt1->getIndex(),nt2->getIndex());
}

/** Return true if nt1 can derive nt2 in one or more steps. 
 **/
bool 
GrammarLevels::canDerive(NonTerminal *nt1, NonTerminal *nt2)
{
  return derivesInMany.getOrder(nt1->getIndex(),nt2->getIndex());
}

/** Calculate the derives-in-many relationship by taking the
 ** transitive closure of the derives-in-one relationship.  Basically,
 ** keep adding transitive relationships until there are no more to
 ** add (fixpoint algorithm).
 **/
void 
GrammarLevels::makeClosure()
{
  derivesInMany = derivesInOne;
  bool changed = false;
  do
    changed = derivesInMany.getClosure();
  while (changed);
}


/** Create a 2D 'derives' table of size NxN, with all entries set to false
 **/
GrammarLevels::DerivesTable::DerivesTable(const int N)
{
  table.resize(N);
  for (int i = 0; i < N; i++) 
  {
    table[i].resize(N);
    for (int j = 0; j < N; j++) 
      table[i][j] = false;
  }
}

/** Copy constructor: create a new table as a copy of dtb
 **/
GrammarLevels::DerivesTable::DerivesTable(const DerivesTable &dtb)
{
  const int N = dtb.table.size();
  table.resize(N);
  for (int i = 0; i < N; i++) 
  {
    table[i].resize(N);
    for (int j = 0; j < N; j++) 
      table[i][j] = dtb.table[i][j];
  }
}

/** Assignment operation between tables: resize the assigned table to
 **the correct size, and copy over the true/false values.
 **/
GrammarLevels::DerivesTable & 
GrammarLevels::DerivesTable::operator=(const DerivesTable &dtb)
{
  const int N = dtb.table.size();
  table.resize(N);
  for (int i = 0; i < N; i++) 
  {
    table[i].resize(N);
    for (int j = 0; j < N; j++) 
      table[i][j] = dtb.table[i][j];
  }
  return *this;
}

/** Calculate one extra step in the transitive closure of a table.  
 ** go through all table elements, and if A->B and B->C, then A->C.
 ** Thus, the table is updates in-place.
 ** Return true if one complete iteration changes anything at all.
 **/
bool 
GrammarLevels::DerivesTable::getClosure()
{
  bool changed_one = false;
  const int N = table.size();

  // For each entry in the table that isn't already 'true':
  for (int i=0; i<N; i++) {
    for (int j=0; j<N; j++) {
      if (! table[i][j]) {
        // See if there exists a k, such that i -> k -> j
        for (int k=0; k<N; k++) {
          if ( table[i][k] && table[k][j] ) {
            table[i][j] = true;
            changed_one = true;
            break;  // No need to look for other k's
          }
        }
      }
    }
  }
  return changed_one;
}

