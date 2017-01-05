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

#ifndef VISITOR_H
#define VISITOR_H

/** \file Visitor.h Completely defines the abstract base class
 ** Visitor, the root class of the visitor hierarchy.
 **/

class RightPart;
class Grammar;
class NonTerminal;
class Terminal;

/** This is a use of the Visitor pattern, to be applied to Rightpart
 ** instances.  This is the abstract base class of all Visitors.  In
 ** each derived visitor class, a separate visitXXX method should be
 ** defined for each of the derived classes of RightPart.
 **/
class Visitor
{
  public:
    virtual void visitUnion(RightPart *, RightPart *) = 0;
    virtual void visitConcat(RightPart *, RightPart *) = 0;
    virtual void visitKleeneClosure(RightPart *) = 0;
    virtual void visitPositiveClosure(RightPart *) = 0;
    virtual void visitOption(RightPart *) = 0;
    virtual void visitTerminal(Terminal *) = 0;
    virtual void visitNonTerminal(NonTerminal *) = 0;
    virtual void visitEmpty() = 0;
 protected:
    Visitor() {}
};


#endif
