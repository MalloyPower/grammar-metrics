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

/** \file CalcLevelsVisitor.h
 ** Completely defines the class CalcLevelsVisitor
 **/

#ifndef CALCLEVELSVISITOR_H
#define CALCLEVELSVISITOR_H

#include <assert.h>
#include "Visitor.h"

/** This visitor calculates the derives-in-one relationship for a
 ** grammar.  Thus, it should be applied to each RHS in turn, and the
 ** cumulative result of this will be the full derives-in-one-step
 ** relationship.
 **/
class CalcLevelsVisitor : public Visitor
{
 public:
  /** Create a visitor; n is the total number of non-terminals.  Use
   ** the same visitor instance for all rules in the grammar. 
   **/
  CalcLevelsVisitor(int n) : currentLhs(NULL) {
    levels = new GrammarLevels(n);
  }
  /** Take a note of the non-terminal on the left-hand-side of the
   ** current grammar rule.  Do this each time before visiting a
   ** rule's RHS.
   **/
  void setLhs(NonTerminal *lhs) 
    { assert(lhs != NULL); currentLhs=lhs; }
  /** Return the grammar levels that have been calculated by this visit.
   ** Only do this after all RHSs have been visited.
   **/
  GrammarLevels * getLevels() 
    { assert(levels != NULL);  return levels; }

  void visitUnion(RightPart *rp1, RightPart *rp2) 
    { rp1->accept(this); rp2->accept(this); }
  void visitConcat(RightPart *rp1, RightPart *rp2) 
    { rp1->accept(this); rp2->accept(this); }
  void visitKleeneClosure(RightPart *rp) 
    { rp->accept(this); }
  void visitPositiveClosure(RightPart *rp) 
    { rp->accept(this); }
  void visitOption(RightPart *rp) 
    { rp->accept(this); }
  void visitEmpty() { ; }
  void visitTerminal(Terminal *) { ; }

  /** This is the only visit method with any real functionality.
   ** If we've got here, we've found a non-t on the RHS, so record
   ** the fact that the non-t on the LHS derives this one.
   **/
  void visitNonTerminal(NonTerminal *nterm)  { 
    assert(currentLhs != NULL);  // Should have set this before visit!
    levels->setDerive(currentLhs, nterm);
  }

 private:
  CalcLevelsVisitor() {}    /** Must always use the other constructor */
  NonTerminal *currentLhs;  /** The non-t on the LHS of this rule */
  GrammarLevels * levels;   /** The levels worked out by this visit */
};

#endif
