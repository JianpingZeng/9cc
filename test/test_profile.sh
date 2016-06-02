#!/bin/bash

outdir=profile

rm -rf $outdir
mkdir $outdir

for src in *.c utils/*.c sys/unix.c sys/linux.c
do
    f1=/tmp/7cc/`basename $src`.s
    f2=$outdir/`basename $src`.profile.txt
    dir=`pwd`
    gmon=gmon.out
    echo "Profiling $src ..."
    ./cc1 -fversion=1 -DCONFIG_LINUX -DCONFIG_COLOR_TERM -DBUILD_DIR='"$dir"' $src -o $f1
    gprof ./cc1 $gmon -p > $f2
done
