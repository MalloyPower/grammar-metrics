# SynQ: SYNtactic Quantification of grammar complexity.

Copyright 2004 
* James F. Power, NUI Maynooth, Ireland <jpower@cs.nuim.ie>
* Brian A. Malloy, Clemson University, Sc, USA <malloy@cs.clemson.edu>

Released under the GNU General Public License.

This version last edited on 16 August 2004, 
* with minor corrections on 09 July 2005.
* updated Dec 2016, fixes to impurity and McCabe metrics.

## BUILD:

To build, change to the src/ directory, and type "make".
Requires gcc, flex and bison.

I've only ever built this on linux boxes but there shouldn't be
anything too unportable in there.


## EXAMPLES:

Sample input files for the tool are in the grammars/ directory.
The grammar format looks roughly like a bison file, 
but it also supports EBNF constructs.


## DOCUMENTATION:

For documentation, point your web browser at html_docs/index.html
The documentation was generated using [doxygen](http://www.stack.nl/~dimitri/doxygen/), version 1.3.5.


[James Power](http://www.cs.nuim.ie/~jpower/), 09 July 2005.

