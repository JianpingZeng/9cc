#!/bin/bash

Assemble () {
    old=`pwd`
    echo "Processing.... $1 > $2"
    rm -rf $2
    mkdir -p $2
    cd $1
    for src in *.c utils/*.c sys/unix.c sys/linux.c
    do
        sfile=$2/`basename $src`.s
        ./7cc -S $src -o $sfile
        if [ $? -eq 0 ]; then
            echo -e "\033[0;32m[OK]\033[0m $src"
        else
            echo -e "\033[0;31m[FAIL]\033[0m $src"
        fi
    done
    cd $old
}

Diff() {
    echo "Diff..."
    for src in $1/*.s
    do
        src2=$2/`basename $src`
        echo "diff $src $src2 ..."
        diff $src $src2 > /dev/null
        if [ $? -eq 0 ]; then
            echo -e "\033[0;32m[OK]\033[0m $src"
        else
            echo -e "\033[0;31m[FAIL]\033[0m $src"
        fi
    done
}

d=`pwd`

Assemble 7cc $d/out/1
Assemble tmp/7cc $d/out/2
Diff $d/out/1 $d/out/2
