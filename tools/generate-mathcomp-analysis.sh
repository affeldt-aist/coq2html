#!/bin/sh
set -eux
MATHCOMP_ANALYSIS=./analysis
COMMIT_HASH=$1
DIR=$(pwd `dirname .`)
OUTDIR=$DIR/html/analysis
INDEX_BLACKLIST_FILE=$DIR/tools/index-blacklist

rm -rf $OUTDIR
mkdir -p $OUTDIR

cd $MATHCOMP_ANALYSIS

ls -l

FILES=$(find . -name "*.v" -or -name "*.glob")

coqdep -f _CoqProject > depend.d
cat -n depend.d >&2
$DIR/ocamldot/ocamldot --style "bgcolor=white; splines=true; nodesep=1; node [fontsize=18, shape=rect, color=\"#dbc3b6\", style=filled];" depend.d > depend.dot

sed -i 's/Classical/mathcomp\.classical/' depend.dot
sed -i 's/Theories/mathcomp\.analysis/' depend.dot
sed -i 's/Reals_stdlib/mathcomp\.reals_stdlib/' depend.dot
sed -i 's/Experimental_reals/mathcomp\.experimental_reals/' depend.dot
sed -i 's/Reals/mathcomp\.reals/' depend.dot
sed -i 's/Analysis_stdlib/mathcomp\.analysis_stdlib/' depend.dot
sed -i 's/\//\./g' depend.dot

$DIR/tools/generate-hierarchy-graph.sh

$DIR/rocqnavi -title "MathComp-Analysis($COMMIT_HASH)" -d $OUTDIR -base mathcomp \
  -Q theories analysis -coqlib https://coq.inria.fr/doc/V8.20.1/stdlib/ \
  -external https://math-comp.github.io/htmldoc_2_1_0/ mathcomp.ssreflect \
  -external https://math-comp.github.io/htmldoc_2_1_0/ mathcomp.algebra \
  -hierarchy-graph "hierarchy-graph.dot" \
  -dependency-graph "depend.dot" \
  -index-blacklist $INDEX_BLACKLIST_FILE \
  $FILES

cp hierarchy-graph.dot depend.dot $OUTDIR/
