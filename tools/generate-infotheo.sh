#!/bin/sh
set -eux
PROJECT=${SOURCE:-"./infotheo"}
REVISION=${REVISION:-"no"}
DIR=$(pwd `dirname .`)
OUTDIR=${OUTDIR:-$DIR/html/infotheo-$REVISION}
CoqProject=${CoqProject:-_CoqProject}

rm -rf $OUTDIR
mkdir -p $OUTDIR

cd $PROJECT

ls -l

FILES=$(find . -name "*.v" -or -name "*.glob")

coqdep -f $CoqProject > depend.d
cat -n depend.d >&2
$DIR/ocamldot/ocamldot --style "bgcolor=white; splines=true; nodesep=1; node [fontsize=18, shape=rect, color=\"#dbc3b6\", style=filled];" depend.d > depend.dot

sed -i 's/lib/infotheo\.lib/' depend.dot
sed -i 's/probability/infotheo\.probability/' depend.dot
sed -i 's/information_theory/infotheo\.information_theory/' depend.dot
sed -i 's/ecc_classic/infotheo\.ecc_classic/' depend.dot
sed -i 's/ecc_modern/infotheo\.ecc_modern/' depend.dot
sed -i 's/robust/infotheo\.robust/' depend.dot
sed -i 's/toy_examples/infotheo\.toy_examples/' depend.dot
sed -i 's/\//\./g' depend.dot

$DIR/rocqnavi -title "Infotheo-$REVISION" -d $OUTDIR -base infotheo \
  -coqlib https://coq.inria.fr/doc/V8.20.1/stdlib/ \
  -external https://math-comp.github.io/htmldoc_2_1_0/ mathcomp.ssreflect \
  -external https://math-comp.github.io/htmldoc_2_1_0/ mathcomp.algebra \
  -external https://math-comp.github.io/analysis/htmldoc_1_9_0/ mathcomp.analysis \
  -dependency-graph "depend.dot" \
  -show-type-information-using-coqtop-process \
  $FILES

