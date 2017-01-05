
LEX = flex
YACC = bison

EXE=synq

CCC = g++
CCFLAGS= -Wall  -Wno-unused-function #-DWANT_ORIGINAL
LINKFLAGS= 

OBJS =	ebnf.tab.o \
	RightPart.o \
	Vocab.o \
	Grammar.o \
	GrammarLevels.o \
	lex.yy.o

$(EXE): $(OBJS) main.o
	$(CCC) -o $(EXE) $(OBJS) main.o $(LINKFLAGS)
	@echo Usage: $(EXE) inputfile.y


main.o: main.cpp Grammar.h
	$(CCC) $(CCFLAGS) -c main.cpp

ebnf.tab.c: ebnf.y
	$(YACC) -d ebnf.y

Grammar.o: Grammar.h Grammar.cpp RightPart.h Visitor.h \
	   CalcLevelsVisitor.h GrammarLevels.h HalsteadVisitor.h
	$(CCC) $(CCFLAGS) -c Grammar.cpp

GrammarLevels.o: GrammarLevels.h Vocab.h GrammarLevels.cpp
	$(CCC) $(CCFLAGS) -c GrammarLevels.cpp

Vocab.o: Vocab.h Vocab.cpp RightPart.h
	$(CCC) $(CCFLAGS) -c Vocab.cpp

RightPart.o: RightPart.cpp RightPart.h Visitor.h
	$(CCC) $(CCFLAGS) -c RightPart.cpp

lex.yy.c: ebnf.l ebnf.tab.h Grammar.h
	$(LEX) ebnf.l

ebnf.tab.o: ebnf.tab.c
	$(CCC) $(CCFLAGS) -c ebnf.tab.c

lex.yy.o: lex.yy.c
	$(CCC) $(CCFLAGS) -c lex.yy.c

clean:
	rm -f *~ *.o y.tab.c y.tab.h ebnf.tab.h ebnf.tab.c lex.yy.c run core



