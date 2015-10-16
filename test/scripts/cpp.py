#!/usr/bin/python
#-*- coding: utf-8 -*-
#

import os
import sys
import subprocess
import tempfile
import re

GCC = "/usr/bin/gcc"
#GCC = os.path.expanduser("~/Downloads/8cc/8cc")
MCC = "./mcc"
SDK_PATH = subprocess.check_output(["xcrun", "--show-sdk-path"]).strip()
SDK_INC = SDK_PATH + "/usr/include"

def get_incs():
    '''get include dirs'''
    dirs = [SDK_INC, "utils", "sys"]
    ret = []
    for dir in dirs:
        ret.append("-I" + dir)
    return ret

def do_process(cmd):
    '''cmd'''
    print cmd
    p = subprocess.Popen(cmd, stdout = subprocess.PIPE, stderr = subprocess.PIPE)
    out, err = p.communicate()
    lines = out.splitlines()
    ret = []

    if p.poll() == 0:
        '''ok'''
        for line in lines:
            line = line.strip()
            if len(line) == 0 or line.startswith('#'):
                continue
            ret.append(line)
        return ret
    else:
        '''fail'''
        print "fail %s" % cmd
        print err
        sys.exit(1)

def gcc_process(file, argv=None):
    '''gcc'''
    gcc_argv = [GCC, "-E", "-DCONFIG_X64", "-U__GNUC__"]

    if argv:
        gcc_argv = gcc_argv + argv
    
    gcc_argv.append(file)

    return do_process(gcc_argv)

def mcc_process(file, argv=None):
    '''mcc'''
    mcc_argv = [MCC, "-E", "-DCONFIG_X64", "-I~/Downloads/8cc/include"]

    if argv:
        mcc_argv = mcc_argv + argv
    
    mcc_argv.append(file)
    
    return do_process(mcc_argv)

def isblank(ch):
    space = [' ', '\t', '\v', '\f']
    for c in space:
        if ch == c:
            return True
    return False
    return ch in space

def line_to_tokens(line):
    '''line to tokens'''
    i = 0
    line = line + "\n"
    end = len(line)
    ret = []
    while True:
        if i >= end:
            break;
        while isblank(line[i]):
            i = i + 1
        if i >= end:
            break;

        ret.append(line[i:1])
        i = i + 1

    return ret

def break_to_tokens(lines):
    '''convert to tokens'''
    tokens = []
    for line in lines:
        toks = line_to_tokens(line)
        if toks:
            for t in toks:
                tokens.append(t)

    return tokens

def compare(file, tokens1, tokens2):
    '''compare tokens'''
    if len(tokens1) != len(tokens2):
        print "length: %d != %d" % (len(tokens1), len(tokens2))

    l = min(len(tokens1), len(tokens2))
    for i in range(l):
        if tokens1[i] != tokens2[i]:
            print "%s: %s != %s" % (file, tokens1[i], tokens2[i])
            sys.exit(1)

def process(file, argv=None):
    '''process'''
    lines1 = gcc_process(file, argv)
    lines2 = mcc_process(file, argv)

    str1 = ""
    for line in lines1:
        str1 = str1 + line + " "
    str1.strip()
    str1 = str1 + "\n"

    str2 = ""
    for line in lines2:
        str2 = str2 + line + " "
    str2.strip()
    str2 = str2 + "\n"

    tokens1 = line_to_tokens(str1)
    tokens2 = line_to_tokens(str2)
    compare(file, tokens1, tokens2)

def dir_srcs(dir):
    '''get dir srcs'''
    return [os.path.join(dir, f) for f in os.listdir(dir) if os.path.isfile(os.path.join(dir, f)) and (f.endswith(".c") or f.endswith(".h"))]

def main():
    '''main'''
    incs = get_incs()
#    incs = incs + ["-DCONFIG_X64"]
    files = dir_srcs(os.getcwd())
    print "%d files" % len(files)
    for file in files:
        process(file, incs)
        print "%s [OK]" % file

if __name__ == "__main__":
    reload(sys)
    sys.setdefaultencoding('utf8')
    sys.exit(main())
