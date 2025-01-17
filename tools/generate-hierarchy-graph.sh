coqtop <<EOF
From HB Require Import structures.
Require Import mathcomp.classical.all_classical.
Require Import mathcomp.analysis.all_analysis.
Require Import mathcomp.reals_stdlib/Rstruct.
Require Import mathcomp.reals.all_reals.
Import mathcomp.analysis.lebesgue_integral.HBSimple.
Import mathcomp.analysis.lebesgue_integral.HBNNSimple.
Require Import mathcomp.analysis_stdlib.Rstruct_topology.
HB.graph "hierarchy-graph.dot".
EOF
