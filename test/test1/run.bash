#!/bin/bash
set -eux

DIR=$(cd $(dirname $0) && pwd)

RocqNavi=$DIR/../../rocqnavi

rm -rf $DIR/html
mkdir $DIR/html

cd $DIR
VFiles="Main.v"
coqc $VFiles

GlobFiles="Main.glob"
$RocqNavi -title "test1" -d ./html $VFiles $GlobFiles \
  -doc-source-url "https://example.com/unexisting/tree/xxxxxxxxxxxxx/"

# Check html files
if command -v xq >/dev/null 2>&1; then
    for exp_html in $DIR/expected_html/*.html
    do
        base=$(basename $exp_html)
        act_html=$DIR/html/$base
        echo "checking $base..."
        if type xq > /dev/null 2>&1; then
            diff -u <(xq $exp_html) <(xq $act_html)
        else
            diff -uw $exp_html $act_html
        fi
    done
else
    # Check other resource files and directories
    diff -ruw -x rocqnavi.css -x rocqnavi.js \
         $DIR/expected_html $DIR/html
fi
