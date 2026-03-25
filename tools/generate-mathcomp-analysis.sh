#!/bin/bash
set -eux
MATHCOMP_ANALYSIS=${MATHCOMP_ANALYSIS:-"$OPAM_SWITCH_PREFIX/lib/coq/user-contrib/mathcomp/"}
REVISION=${REVISION:-"no"}
DIR=$(pwd `dirname .`)
OUTDIR=${OUTDIR:-$DIR/html/analysis-$REVISION}
ANALYSIS_SRC=${ANALYSIS_SRC:-$OPAM_SWITCH_PREFIX/.opam-switch/sources/coq-mathcomp-analysis.1.16.0}
INDEX_BLACKLIST_FILE=$DIR/sample_blacklist/index_blacklist

rm -rf $OUTDIR
mkdir -p $OUTDIR

SRC=$OUTDIR/src
mkdir $SRC

cp -r $ANALYSIS_SRC/* $SRC/

cd $SRC

ls -l

coqdep -f _CoqProject > depend.d
cat -n depend.d >&2

$DIR/tools/generate-hierarchy-graph.sh

cp hierarchy-graph.dot depend.d $OUTDIR/


cd $MATHCOMP_ANALYSIS
FILES=$(find analysis/topology_theory -name "*.v" -or -name "*.glob")

$DIR/rocqnavi -title "MathComp-Analysis-$REVISION" -d $OUTDIR \
  -debug \
  -coqlib https://coq.inria.fr/doc/V8.20.1/stdlib/ \
  -Q analysis mathcomp.analysis \
  -Q classical mathcomp.classical \
  -Q reals mathcomp.reals \
  -Q reals_stdlib mathcomp.reals_stdlib \
  -Q experimental_reals mathcomp.experimental_reals \
  -Q theories mathcomp.analysis \
  -Q analysis_stdlib mathcomp.analysis_stdlib \
  -external https://math-comp.github.io/htmldoc_2_5_0/ mathcomp.ssreflect \
  -external https://math-comp.github.io/htmldoc_2_5_0/ mathcomp.algebra \
  -external https://math-comp.github.io/htmldoc_2_5_0/ mathcomp.order \
  -structure-graph $OUTDIR/"hierarchy-graph.dot" \
  -file-graph-from-depend $OUTDIR/"depend.d" \
  -index-blacklist $INDEX_BLACKLIST_FILE \
  -show-type-information-using-coqtop-process \
  -doc-source-url "https://github.com/math-comp/analysis/tree/859965addfaf30afc56c2f1d291db2a0df940d95/" \
  $FILES
