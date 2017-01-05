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

#ifndef GRAMMARLEVELS_H
#define GRAMMARLEVELS_H

/** \file GrammarLevels.h
 ** Defines the classes GrammarLevels and GrammarLevels::DerivesTable
 **/

#include <vector>
#include <iostream>
#include <iomanip>
using namespace std;

/** This class represents the derives-in-one and derives-in-many
 ** relationship between non-terminals.
 ** For non-terminals A and B
 ** derives-in-one holds if B is on the RHS of the rule for A.
 ** derives-in-many holds if B can be dreived from A in one or more steps.
 **/
class GrammarLevels {

 public:
  /** n should be the number of non-terminals in the grammar */
  GrammarLevels(int n) : derivesInOne(n), derivesInMany(n) {} 
  friend ostream & operator<<(ostream &, const GrammarLevels &);
  void setDerive(NonTerminal *, NonTerminal *);

  bool areEquiv(NonTerminal *, NonTerminal *);
  bool canDerive(NonTerminal *, NonTerminal *);

  void makeClosure();
  int getDerivesInOneEdges() { return derivesInOne.getNumEdges(); } 
  int getDerivesInManyEdges() { return derivesInMany.getNumEdges(); } 

 private:
  /** This class represents a "derives" relation between two
   ** non-terminals as a NxN matrix of booleans.  The indexes into the
   ** matrix correspond to the indexes of the non-terminal in the
   ** grammar.
   **/
  class DerivesTable
  {
    public:
      DerivesTable(int);
      DerivesTable(const DerivesTable &);
      DerivesTable & operator=(const DerivesTable &);

      /** Print out the table; use a '*' to denote the relation holds */
      friend ostream & operator<<(ostream &out, const DerivesTable &dt)
      {
	const int N = dt.table.size();
        for (int i = 0; i < N; i++) {
	  out << setw(3) << i << ": ";
          for (int j = 0; j < N; j++) {
	    out << (dt.table[i][j] ? "*" : " ");
	  }
          out << endl;
        }
        return out;
      }

      /* Set the "x derives y" value to true */
      void setOrder(int x, int y) { table[x][y]=true; }
      /* Return true iff x derives y */
      bool getOrder(int x, int y) { return table[x][y]; }
      /** Two non-terminals are equiv if x derives y and y derives x */
      bool areEquiv(int x, int y) { return table[x][y] && table[y][x]; }

      bool getClosure();

      /** Return the number of edges in the graph */
      int getNumEdges()
      {
	const int N = table.size();
	int edgeCount=0;
        for (int i = 0; i < N; i++) {
          for (int j = 0; j < N; j++) {
	    if (table[i][j] && i!=j)
	      edgeCount++;
	  }
        }
	return edgeCount;
      }


    private:
      /** For simplicity we represent the 2D matrix as a vector of
       ** vectors.  Two non-terminal indexes will then yield a bool,
       ** indicating the derives relationship.
       **/
      vector< vector<bool> > table;
  };

  /** Whether a non-t derives another in exactly one step */
  DerivesTable derivesInOne;

  /** Whether a non-t derives another in one or more steps */
  DerivesTable derivesInMany;

};




#endif
