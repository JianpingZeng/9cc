#!/bin/bash

for src in *.c utils/*.c sys/*.c
do
    f1=/tmp/`basename $src`.1.i
    f2=/tmp/`basename $src`.2.i
    ./cc1_stage1 -E -DBUILD_DIR='"/mnt/hgfs/下载/7cc"' $src > $f1
    ./cc1_stage2 -E -DBUILD_DIR='"/mnt/hgfs/下载/7cc"' $src > $f2
    diff $f1 $f2
    if [ $? -eq 0 ]; then
       echo "[PASS] $src"
    else
       echo "[FAIL] $src"
    fi
done

