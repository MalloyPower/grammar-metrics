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

#ifndef SIZEVISITOR_H
#define SIZEVISITOR_H

/** \file SizeVisitor.h
 ** Completely defines the class SizeVisitor
 **/

#include "Visitor.h"

/** This visitor calculates the size of a RHS, which is simply a
 ** count of the number of terminals and non-terminals that occur
 ** there.
 **/
class SizeVisitor : public Visitor
{
  public:
     SizeVisitor() : theSize(0) { }
     /** Reset the size value, so we can re-use this visitor on another RHS */
     void reInit() { theSize=0; }
     /** Return the size value: use this after the visit is finished */
     int getSize() { return theSize; }

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
     void visitTerminal(Terminal *) { theSize++; }
     void visitNonTerminal(NonTerminal *nterm) { theSize++; }
     void visitEmpty() { ; }

 private:
     /** The number of terminals & non-terms encountered during this visit */
    int theSize;

};

#endif
