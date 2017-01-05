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

#ifndef MCCABEVISITOR_H
#define MCCABEVISITOR_H

/** \file McCabeVisitor.h
 ** Completely defines the class McCabeVisitor
 **/

#include "Visitor.h"

/** This visitor calculates the McCabe complexity of the RHS to which
 ** it is applied.  The McCabe complexity is simply a count of the
 ** number of decisions; thus the union and option operator, along
 ** with the two closure operations all represent a single decision.
 **/
class McCabeVisitor : public Visitor
{
  public:
     McCabeVisitor() : decisionCount(0) { }
     /** Reset the McCabe value, so we can re-use this visitor */
     void reInit() { decisionCount=0; }
     /** Return the McCabe complexity: use this after the visit is finished */
     /** DEc 2016: changed this to add 1 to the count of decisions **/
#ifdef WANT_ORIGINAL
     int getMcCabe() { return decisionCount; }
#else
     int getMcCabe() { return 1+decisionCount; }
#endif

     void visitUnion(RightPart *rp1, RightPart *rp2) 
       { decisionCount++; rp1->accept(this); rp2->accept(this); }
     void visitConcat(RightPart *rp1, RightPart *rp2) 
       { rp1->accept(this); rp2->accept(this); }
     void visitKleeneClosure(RightPart *rp) 
       { decisionCount++; rp->accept(this); }
     void visitPositiveClosure(RightPart *rp) 
       { decisionCount++; rp->accept(this); }
     void visitOption(RightPart *rp) 
       { decisionCount++; rp->accept(this); }
     void visitTerminal(Terminal *) { ; }
     void visitNonTerminal(NonTerminal *nterm) { ; }
     void visitEmpty() { ; }

 private:
     /** The number of decisions encountered during this visit */
    int decisionCount;

};

#endif
