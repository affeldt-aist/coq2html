#!/bin/sh
set -eux

DIR=$(cd $(dirname $0) && pwd)

RocqNavi=$DIR/../../coq2html

rm -rf $DIR/html
mkdir $DIR/html

cd $DIR
VFiles="Main.v"
coqc $VFiles

GlobFiles="Main.glob"
$RocqNavi -title "test1" -d ./html $VFiles $GlobFiles

diff -r --exclude=coq2html.css --exclude=coq2html.js \
  $DIR/expected_html $DIR/html
