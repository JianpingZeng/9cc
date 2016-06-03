#!/bin/bash

outdir=profile
tmpdir=/tmp/7cc

rm -rf $outdir
rm -rf $tmpdir
mkdir $outdir
mkdir $tmpdir

for src in *.c utils/*.c sys/unix.c sys/linux.c
do
    f1=$tmpdir/`basename $src`.s
    f2=$outdir/`basename $src`.profile.txt
    dir=`pwd`
    echo "Profiling $src ..."
    ./cc1 -fversion=1 -DCONFIG_LINUX -DCONFIG_COLOR_TERM -DBUILD_DIR='"$dir"' $src -o $f1
    gprof ./cc1 gmon.out -p > $f2
done
