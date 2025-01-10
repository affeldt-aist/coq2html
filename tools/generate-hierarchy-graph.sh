set -eux
DIR=$(pwd `dirname 0`)
coqtop <<EOF
From HB Require Import structures.
Require Import mathcomp.classical.all_classical.
Require Import mathcomp.reals_stdlib/Rstruct.
Require Import mathcomp.reals.all_reals.
Require Import mathcomp.analysis_stdlib.Rstruct_topology.
Require Import mathcomp.analysis.all_analysis.
HB.graph "$DIR/hierarchy-graph0.dot".
EOF
ccomps -x $DIR/hierarchy-graph0.dot | tred | dot | gvpack -array3 > $DIR/hierarchy-graph.dot
