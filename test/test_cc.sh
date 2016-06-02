#!/bin/bash

ok=0
fail=0

rm -rf /tmp/7cc
mkdir /tmp/7cc

for src in *.c utils/*.c sys/unix.c sys/linux.c
do
    f1=/tmp/7cc/`basename $src`.1.s
    f2=/tmp/7cc/`basename $src`.2.s
    dir=`pwd`
    ./cc1_stage1 -fversion=1 -DCONFIG_LINUX -DCONFIG_COLOR_TERM -DBUILD_DIR='"$dir"' $src -o $f1
    ./cc1_stage2 -fversion=1 -DCONFIG_LINUX -DCONFIG_COLOR_TERM -DBUILD_DIR='"$dir"' $src -o $f2

    if [ "$1" == "-v" ]; then
        diff $f1 $f2
    else
        diff $f1 $f2 > /dev/null
    fi

    if [ $? -eq 0 ]; then
        echo -e "\033[0;32m[PASS]\033[0m $src"
        let "ok += 1"
    else
        echo -e "\033[0;31m[FAIL]\033[0m $src"
        let "fail += 1"
    fi
done

echo "$ok success, $fail failed."
