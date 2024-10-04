#!/bin/sh
set -eux
MATHCOMP_ANALYSIS=./analysis
DIR=$(pwd `dirname .`)
OUTDIR=$DIR/html

rm -rf $OUTDIR
mkdir $OUTDIR

cd $MATHCOMP_ANALYSIS

ls -l

FILES=$(find classical/ theories/ -name "*.v" -or -name "*.glob")

$DIR/tools/generate-hierarchy-graph.sh

$DIR/coq2html -title "Mathcomp Analysis" -d $OUTDIR -base mathcomp \
  -Q theories analysis -coqlib https://coq.inria.fr/doc/V8.18.0/stdlib/ \
  -external https://math-comp.github.io/htmldoc_2_1_0/ mathcomp.ssreflect \
  -external https://math-comp.github.io/htmldoc_2_1_0/ mathcomp.algebra \
  -hierarchy-graph "hierarchy-graph.dot" \
  $FILES
