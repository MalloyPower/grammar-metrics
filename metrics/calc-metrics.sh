#!/bin/bash

SRC_DIR=../src
PARSER_DIR=../grammars/cpp-parsers
GRAMMAR_DIR=../grammars/standards

SYNQ=${SRC_DIR}/synq

pushd ${SRC_DIR}
make
popd

for d in ${PARSER_DIR} ${GRAMMAR_DIR}; do
  for f in $d/*.y; do
    echo Measuring $f
    mfile=`basename $f .y`.txt
    ${SYNQ} $f >$mfile
  done
done

