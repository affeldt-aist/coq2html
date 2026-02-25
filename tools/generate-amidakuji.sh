#!/bin/sh
set -eux
PROJECT=./rocq-amidakuji
COMMIT_HASH=${1:-hoge}
DIR=$(pwd `dirname .`)
OUTDIR=$DIR/html/rocq-amidakuji

rm -rf $OUTDIR
mkdir -p $OUTDIR

cd $PROJECT

ls -l

FILES=$(find . -name "*.v" -or -name "*.glob")

coqdep -f _CoqProject > depend.d
cat -n depend.d >&2
#$DIR/ocamldot/ocamldot --style "bgcolor=white; splines=true; nodesep=1; node [fontsize=18, shape=rect, color=\"#dbc3b6\", style=filled];" depend.d > depend.dot

#sed -i 's/Src\//T\./' depend.dot

$DIR/rocqnavi -title "rocqnavi-sample ($COMMIT_HASH)" -d $OUTDIR \
  -debug \
  -coqlib https://coq.inria.fr/doc/V8.20.1/stdlib/ \
  -file-graph-from-depend "depend.d" \
  -Q src T \
  -show-type-information-using-coqtop-process \
  -doc-source-url "https://github.com/yoshihiro503/rocq-amidakuji/tree/252d0b7e02d2d879773f3e6e6386f9a09b2b530c/" \
  $FILES

