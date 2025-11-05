#!/bin/bash
set -eu

DIR=$(cd $(dirname $0) && pwd)

RocqNavi=$DIR/../../rocqnavi

rm -rf $DIR/html
mkdir $DIR/html

cd $DIR
VFiles="Main.v"
coqc $VFiles

GlobFiles="Main.glob"
$RocqNavi -title "test_type_tooltip" -d ./html $VFiles $GlobFiles \
    -show-type-information-using-rocq-lsp


# Check html files
for exp_html in $DIR/expected_html/*.html
do
    base=$(basename $exp_html)
    act_html=$DIR/html/$base
    echo "checking $base..."
    if type xq > /dev/null 2>&1; then
        diff <(xq $exp_html) <(xq $act_html)
    else
        diff $exp_html $act_html
    fi
done

# Check other resource files and directories
diff -r --exclude=rocqnavi.css --exclude=rocqnavig.js --exclude=*.html \
  $DIR/expected_html $DIR/html
