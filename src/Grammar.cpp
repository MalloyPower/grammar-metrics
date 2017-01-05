/** 
 ** SynQ: SYNtactic Quantification of grammar complexity.
 ** Copyright 2004 James F. Power, NUI Maynooth, Ireland <jpower@cs.nuim.ie>
 ** This version: 16 August 2004
 **
 ** This program is free software; you can redistribute it and/or modify
 ** it under the terms of the GNU General Public License as published by
 ** the Free Software Foundation; either version 2 of the License, or
 ** (at your option) any later version.
 **
 ** This program is distributed in the hope that it will be useful,
 ** but WITHOUT ANY WARRANTY; without even the implied warranty of
 ** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ** GNU General Public License for more details.
 **
 ** You should have received a copy of the GNU General Public License
 ** along with this program; if not, write to the Free Software
 ** Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 **/

#include <algorithm>
#include <cmath>   /* For log10 */
#include <assert.h>

#include "Grammar.h"
#include "CalcLevelsVisitor.h"
#include "SizeVisitor.h"
#include "McCabeVisitor.h"
#include "HalsteadVisitor.h"

/** \file Grammar.cpp 
 ** Defines the principal methods in the Grammar
 ** class, including those that initiate the collection of the various
 ** methods.
 **/

Terminal *
Grammar::addTerminal(const string & symbolName) 
{
  terminals.push_back( new Terminal(symbolName) );
  return terminals.back();
}

/** Add a new non terminal to the grammar.  If it's the first
 ** non-terminal we've seen, make it the start symbol.
 **/
NonTerminal *
Grammar::addNonTerminal(const string & symbolName) 
{
  NonTerminal * newSymbol = new NonTerminal(symbolName,prodRules.size());
  if (startSymbol == NULL) 
    startSymbol = newSymbol;
  prodRules.push_back( newSymbol );
  return newSymbol;
}

/** Get the terminal symbol corresponding to this name.
 ** Return NULL if not found.
 **/
Terminal *
Grammar::getTerminal(const string & symbolName) 
{
  // first see if it's a terminal
  Terminal::IsEqual eq(symbolName);
  TermIter term_ptr = find_if(terminals.begin(), terminals.end(), eq);
  if (term_ptr == terminals.end()) 
    return NULL;
  else
    return *term_ptr;
}

/** Get the non-terminal symbol corresponding to this name.
 ** Return NULL if not found.
 **/
NonTerminal *
Grammar::getNonTerminal(const string & symbolName) 
{
  // first see if it's a non-terminal
  NonTerminal::IsEqual eq(symbolName);
  NontIter nterm_ptr = find_if(prodRules.begin(), prodRules.end(), eq);
  if (nterm_ptr == prodRules.end()) 
    return NULL;
  else
    return *nterm_ptr;
}

/** Print out the non-terminals that have no definition (no RHS).
 ** Return a count of these non-terminals.
**/
int 
Grammar::printUndefined(std::ostream & out)
{
  int numUndefined = 0;
  for (NontIter normPtr = prodRules.begin();  
       normPtr != prodRules.end(); ++normPtr) {
    NonTerminal* nt = (*normPtr);
    if (! nt->isDefined()) {
      if (numUndefined == 0)
	out << "Undefined non-terminals:" << std::endl;
      ++numUndefined;
      out << nt->getName() << std::endl;
    }
  }
  return numUndefined;
}

/** Work out the derives-in-many relationship between non-terminals.
 ** This is done by getting the closure of the grammatical levels.
 ** The levels are calcuated now if they have not already been.
 **/
void 
Grammar::makeDerivesRelation()
{
  if (levels == NULL)  {
    calcDerivesInOne();
    assert(levels != NULL);
    levels->makeClosure();
  }
}

/** Print the derives-in-many relationship **/
void 
Grammar::printDerivesRelation(std::ostream & out)
{
  assert(levels != NULL);
  out << "GRAMMATICAL LEVELS:" << std::endl;
  out << (*levels);
}

/** Work out the set of equivalence classes based on the
 ** derives-in-many relationship.  The result is stored in the
 ** equivClasses instance variable, where each equivalence class is a
 ** list of non-terminals.  The derives-in-many tables are calcuated
 ** if they haven't been already.
 **/
void 
Grammar::makeEquivClasses()
{
  if (levels == NULL)
    makeDerivesRelation();
  // For each non-terminal, put it in the correct equivalence class:
  for (unsigned int i=0; i<prodRules.size(); i++) { 
    NonTerminal *nt = prodRules[i];
    bool foundClass = false;
    // See if nt belongs in an existing equivalence class:
    for (unsigned int j=0; j<equivClasses.size(); j++) { 
      if (!equivClasses[j].empty() 
          && levels->areEquiv(nt,equivClasses[j][0])) {
        // Non-terminal nt belongs to equivalence class no. j:
        equivClasses[j].push_back(nt);
        foundClass = true;
        break;
      }
    }
    if (! foundClass) {
      // Non-terminal nt belongs in a new equivalence class of its own:
      vector<NonTerminal *> newClass;
      newClass.push_back(nt);
      equivClasses.push_back(newClass);
    }
  }
}

/** Print the list of non-terminals in each equivalence class.
 ** Assumes that the classes have already been calcuated and stired in
 ** the equivClasses instance variable.
 **/
void 
Grammar::printEquivClasses(std::ostream & out)
{
  const int LINE_SIZE = 7; // No. of non-terminals per line
  for (unsigned int i = 0; i < equivClasses.size(); i++)  {
    if (equivClasses[i].size() == 1)
      continue;
    out << "Class #" << i << ": ";
    if (equivClasses[i].size() > 1)
      out << "(size = " << equivClasses[i].size() << ") ";
    for (unsigned int j = 0; j < equivClasses[i].size(); j++)   {
      if (j % LINE_SIZE == 0) out << std::endl;
      out << "\t" << equivClasses[i][j]->getName();
    }
    out << std::endl;  
  }
}

/** Print the sizes of the equivalence classes, one per line.
 **/
void 
Grammar::printEquivClassSizes(std::ostream & out)
{
  for (unsigned int i = 0; i < equivClasses.size(); i++)
    out << equivClasses[i].size() << std::endl;
}


/** Count and return the number of equivalence classes of size > minsize.
 ** So countEquivClasses(0) counts them all.
 ** The size of an equiv. class is the number of non-terminals it contains.
 **/
int 
Grammar::countEquivClasses(int minsize)
{
  int count = 0;
  for (unsigned int i = 0; i < equivClasses.size(); i++) {
    if (equivClasses[i].size() > (unsigned)minsize)
      count++;
  }
  return count;
}

/** Return the number of equivalence classes, expressed as a
 ** percentage of the number of non-terminals.
 **/
float 
Grammar::countNormEquivClasses()
{
  assert(! prodRules.empty());
  float ans = countEquivClasses(0) * 100.0 / countNonTerminals();
  return ans;
}

/** Find the largest equivalence class and return its size.
 ** Its size is the number of non-terminals it contains.
 **/
int 
Grammar::getLargestEquivClassSize()
{
  unsigned int largest = 0;
  for (unsigned int i = 0; i < equivClasses.size(); i++) {
    if (equivClasses[i].size() > largest)
      largest = equivClasses[i].size();
  }
  return (int)largest;
}


/** Return the average number of symbols on the RHS of the grammar
 ** rules.  This is done by applying the Size Visitor to the RHS of
 ** each non-terminal in turn.
**/
float 
Grammar::averageSize()
{
  assert(! prodRules.empty());
  float totalSize = 0.0;
  SizeVisitor sv;
  for(NontIter normPtr = prodRules.begin();
      normPtr != prodRules.end(); ++normPtr) {
    sv.reInit();
    (*normPtr)->accept(&sv);
    totalSize += sv.getSize();
  }
  return totalSize / prodRules.size();
}

/** Return the Fenton tree impurity value for the grammar.  
 ** This is calcuated over the graph where the nodes are non-terminals
 ** and the edges represent the derives-in-one relationship.
 ** The levels variable must already have been calculated.
 **/
float 
Grammar::getOneImpurity()
{
  assert(prodRules.size() > 1);
  const int N = prodRules.size();
  const int E_ONE = levels->getDerivesInOneEdges();
  //  out << "!!! E is " << E_ONE << " and N is " << N << std::endl;  
  // 02/Sep/2016: Changed (N-1)*(N-1) to N*(N-1) in the denominator below:
#ifdef WANT_ORIGINAL
  float impurity = (100.0 * (E_ONE - N + 1)) / ((N-1)*(N-1));
#else
  float impurity = (100.0 * (E_ONE - N + 1)) / (N*(N-1));
#endif
  return impurity;
}

/** Return the Fenton "closed" tree impurity value for the grammar.  
 ** This is calcuated over the graph where the nodes are non-terminals
 ** and the edges represent the derives-in-many relationship.
 ** The levels variable must already have been calculated.
 **/
float 
Grammar::getManyImpurity()
{
  assert(prodRules.size() > 1);
  const int N = prodRules.size();
  const int E_MANY = levels->getDerivesInManyEdges();
  //  out << "!!! E is " << E_MANY << " and N is " << N << std::endl;  
  float impurity = (100.0 * (E_MANY - N + 1)) / ((N-1)*(N-1));
  return impurity;
}


/** Work out the derives-in-one relation.  
 ** Applies the CalcLevelsVisitor to the rhs of each non-terminal in
 ** turn, and stores the result in the levels instance variable.
 **/
void 
Grammar::calcDerivesInOne()
{
  assert (levels == NULL);
  CalcLevelsVisitor *clv = new CalcLevelsVisitor(prodRules.size());
  for(NontIter normPtr = prodRules.begin();
      normPtr != prodRules.end(); ++normPtr) {
    clv->setLhs(*normPtr);
    (*normPtr)->accept(clv);
  }
  levels = clv->getLevels();
  assert (levels != NULL);
}

/** Create the equivalence graph from the equivalence classes.  This
 ** is a NxN matrix (N=no. of equiv classes) where a boolean indicates
 ** a dervies relationship between the classes.
 ** Result is stored in the equivGraph instance variable.
 **/
void 
Grammar::makeEquivGraph()
{
  if (! equivGraph.empty()) // Already done this
    return;
  // Initialise the equivClasses variable to be a NxN matrix
  const int N = equivClasses.size();
  equivGraph.resize(N);
  // Fill in the edges
  for (int i=0; i<N; i++)  {
    equivGraph[i].resize(N);
    for (int j=0; j<N; j++)  {
      if (i != j)      {
	equivGraph[i][j] = 
          levels->canDerive(equivClasses[i][0],equivClasses[j][0]) ;
      }
      else // No cycles allowed:
	equivGraph[i][j] = false;
    }    
  }
  assert(equivGraph.size() == equivClasses.size());
}

/** Print the graph showing the derives relationship between
 ** equivalence classes.
 **/
void 
Grammar::printEquivGraph(std::ostream &out)
{
  const int N = equivGraph.size();
  out << "EQUIVALENCE GRAPH (size " << N << ")" << std::endl;
  for (int i=0; i<N; i++)  {
    out << i << ": ";
    for (int j=0; j<N; j++)
      out << (equivGraph[i][j] ? "*" : " ");
    out << std::endl;
  }
}

/** Return the McCabe complexity value for the whole grammar.  Creates
 ** a McCabeVisitor to work this out for each RHS, and stores the
 ** result in the localMcCabe instance variable.
 **/
int 
Grammar::calcMcCabe()
{
  int total = 0;  // The sum of the McCabe value for each RHS.
  const int N = prodRules.size();
  localMcCabe.resize(N);
  McCabeVisitor mcv;
  for(int i=0; i<N; i++)  {
    mcv.reInit();
    prodRules[i]->accept(&mcv);
    localMcCabe[i] = mcv.getMcCabe();
    total += localMcCabe[i];
  }
  return total;
}

/** Print the McCabe complexity value for each non-terminal.  That is,
 ** the McCabe value for its RHS.  Assumes that these have already
 ** been calculated and stored in the localMcCabe instance variable.
 **/
void 
Grammar::printMcCabe(std::ostream & out)
{
  assert(localMcCabe.size() == prodRules.size());
  const int LINE_SIZE = 20;
  out << "McCabe Metrics:";
  for (unsigned int i = 0; i < localMcCabe.size(); i++)  {
    if (i % LINE_SIZE == 0) out << std::endl;
    out << setw(4) << localMcCabe[i];
  }
  out << std::endl;
}


static float 
my_log2(float x)
{
  return log10(x) / log10(2.0);
}

/** Return the Halstead volume metric for the grammar.
 ** Uses a HalsteadVisitor to count the no. of operators and operands,
 ** and then applies Halstead's formula.
 **/
float 
Grammar::calcHalstead()
{
  assert (! prodRules.empty());
  HalsteadVisitor hv;
  for(unsigned int i=0; i<prodRules.size(); i++)
    prodRules[i]->accept(&hv);
  // eta_1 = No. of distinct operators:
  float eta_1 = 6; 
  // eta_2 = No. of distinct operands
  float eta_2 = countTerminals() + countNonTerminals();
  assert(eta_2 > 0);
  // n_1 = total number of operators
  float n_1 = hv.getOperatorCount();
  // n_2 = total number of operands
  float n_2 = hv.getOperandCount() + prodRules.size();
  // Now calculate Halstead's volume:
  float e = ( eta_1 * n_2 * (n_1+n_2) * my_log2(eta_1+eta_2) ) / (2.0*eta_2);
  return e;
}

/** Return the Varju height metric from the grammar.  This is the
 ** maximum distance of any non-terminal from the start symbol, and is
 ** expressed as a percentage if the number of equivalence classes.
 **/
float 
Grammar::calcHeight()
{
  assert (! equivClasses.empty());
  // Set up storage for the height metric of each class:
  const int N = equivClasses.size();
  varjuHeight.resize(N);
  for (int i=0; i<N; i++)
    varjuHeight[i] = 0;
  makeEquivGraph(); // In case it's not laready made
  // Kick off the recursive calculation of the height metric:
  float height = calcGlobalHeight(0,1); 
  // ... since Class 0 (for start symbol) has height 1
  float height_percent = height * 100.0 / N;
  return height_percent; 
}

static int 
my_max(int x, int y)
{
  return (x>y ? x : y);
}

/** Assign a new height to an equivalence class.  
 ** This new height is then propagated to the children and
 ** descendents of this equivalence class.
 ** i.e. this is a pre-opder recursive traversal of the equivalence graph;
 ** equivalence graph must be - and is - an (acyclic) tree.
 ** Return the maximum height among this class' children.
 **/
int 
Grammar::calcGlobalHeight(int classNum, int height)
{
  assert(varjuHeight.size() == equivClasses.size());
  // Only update if I have a new max height value for this class:
  if (varjuHeight[classNum] >= height)
    return varjuHeight[classNum];
  varjuHeight[classNum] = height;
  // Now I must update all my children with my (new) height plus one:
  int maxHeight=0;
  for (unsigned int i=0; i<equivClasses.size(); i++)  {
    if (equivGraph[classNum][i]) { // then i is my child, so...
      // tell my child that its height is now mine plus one:
      maxHeight = my_max(maxHeight,calcGlobalHeight(i,height+1));
    }
  }
  return maxHeight;  // Max height among my children.
}


