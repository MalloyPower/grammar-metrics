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

#ifndef HALSTEADVISITOR_H
#define HALSTEADVISITOR_H

/** \file HalsteadVisitor.h
 ** Completely defines the class HalsteadVisitor
 **/

#include "Visitor.h"

/** This visitor is used in calculating the Halstead volume metric for
 ** a RHS.  It actually just counts the operators and operands on a
 ** RHS.  Use this by applying the visitor and then using Halstead's
 ** formula on the counts.  Union, concatension, option and the
 ** closures count as operators; terminals and non-terminals count as
 ** operands.  At the moment I count the empty string as an operator,
 ** though perhaps this should be an operand (?).
 **/
class HalsteadVisitor : public Visitor
{
  public:
     HalsteadVisitor() : operatorCount(0), operandCount(0) { }
     /** Reset the counts so we can re-use this visitor on another RHS */
     void reInit() { operatorCount = operandCount = 0; }
     /** Return the no. of operators found during the visit */
     int getOperatorCount() const { return operatorCount; }
     /** Return the no. of operands found during the visit */
     int getOperandCount() const { return operandCount; }

     void visitUnion(RightPart *rp1, RightPart *rp2) 
       { operatorCount++; rp1->accept(this); rp2->accept(this); }
     void visitConcat(RightPart *rp1, RightPart *rp2) 
       { operatorCount++; rp1->accept(this); rp2->accept(this); }
     void visitKleeneClosure(RightPart *rp) 
       { operatorCount++; rp->accept(this); }
     void visitPositiveClosure(RightPart *rp) 
       { operatorCount++; rp->accept(this); }
     void visitOption(RightPart *rp) 
       { operatorCount++; rp->accept(this); }
     /** A terminal symbol is an operand */
     void visitTerminal(Terminal *) { operandCount++; }
     /** A non-terminal symbol is an operand */
     void visitNonTerminal(NonTerminal *nterm) { operandCount++; }
     /** The empty string is counted as an operator */
     void visitEmpty() { operatorCount++; }  // or operandCount++ ???

 private:
     /** The number of (rightpart) operators encountered during this visit */
    int operatorCount;
     /** The number of operands encountered during this visit */
    int operandCount;

};

#endif
