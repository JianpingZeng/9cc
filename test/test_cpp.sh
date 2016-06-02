#!/bin/bash

ok=0
fail=0

rm -rf /tmp/7cc
mkdir /tmp/7cc

for src in *.c utils/*.c sys/unix.c sys/linux.c
do
    f1=/tmp/`basename $src`.1.i
    f2=/tmp/`basename $src`.2.i
    ./cc1_stage1 -E -DBUILD_DIR='"/mnt/hgfs/下载/7cc"' $src > $f1
    ./cc1_stage2 -E -DBUILD_DIR='"/mnt/hgfs/下载/7cc"' $src > $f2
    diff $f1 $f2 > /dev/null
    if [ $? -eq 0 ]; then
        echo -e "\033[0;32m[PASS]\033[0m $src"
        let "ok += 1"
    else
        echo -e "\033[0;31m[FAIL]\033[0m $src"
        let "fail += 1"
    fi
done

echo "$ok success, $fail failed."

