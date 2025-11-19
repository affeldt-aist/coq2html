#!/bin/bash
set -eux
MATHCOMP_ANALYSIS=${MATHCOMP_ANALYSIS:-"$OPAM_SWITCH_PREFIX/lib/coq/user-contrib/mathcomp/"}
REVISION=${REVISION:-"no"}
DIR=$(pwd `dirname .`)
OUTDIR=${OUTDIR:-$DIR/html/analysis-$REVISION}
ANALYSIS_SRC=${ANALYSIS_SRC:-$OPAM_SWITCH_PREFIX/.opam-switch/sources/coq-mathcomp-analysis.dev}
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
$DIR/ocamldot/ocamldot --style "bgcolor=white; splines=true; nodesep=1; node [fontsize=18, shape=rect, color=\"#dbc3b6\", style=filled];" depend.d > depend.dot
cat -n depend.dot >&2

sed -i 's|classical/|mathcomp.classical.|' depend.dot
sed -i 's|theories/|mathcomp.analysis.|' depend.dot
sed -i 's|reals_stdlib/|mathcomp.reals_stdlib.|' depend.dot
sed -i 's|experimental_reals/|mathcomp.experimental_reals.|' depend.dot
sed -i 's|reals/|mathcomp.reals.|' depend.dot
sed -i 's|analysis_stdlib/|mathcomp.analysis_stdlib.|' depend.dot
sed -i 's|/|.|g' depend.dot

cat -n depend.dot >&2
$DIR/tools/generate-hierarchy-graph.sh

cp hierarchy-graph.dot depend.dot $OUTDIR/


cd $MATHCOMP_ANALYSIS
FILES=$(find analysis/topology_theory -name "*.v" -or -name "*.glob")

$DIR/rocqnavi -title "MathComp-Analysis-$REVISION" -d $OUTDIR \
  -coqlib https://coq.inria.fr/doc/V8.20.1/stdlib/ \
  -Q analysis mathcomp.analysis \
  -Q classical mathcomp.classical \
  -Q reals mathcomp.reals \
  -Q reals_stdlib mathcomp.reals_stdlib \
  -Q experimental_reals mathcomp.experimental_reals \
  -Q theories mathcomp.analysis \
  -Q analysis_stdlib mathcomp.analysis_stdlib \
  -external https://math-comp.github.io/htmldoc_2_1_0/ mathcomp.ssreflect \
  -external https://math-comp.github.io/htmldoc_2_1_0/ mathcomp.algebra \
  -structure-graph $OUTDIR/"hierarchy-graph.dot" \
  -file-graph $OUTDIR/"depend.dot" \
  -index-blacklist $INDEX_BLACKLIST_FILE \
  -show-type-information-using-rocq-lsp \
  $FILES
