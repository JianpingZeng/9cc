#!/usr/bin/python
#-*- coding: utf-8 -*-
#

import os
import sys
import subprocess
import tempfile
import re

GCC = "/usr/bin/gcc"
MCC = "./mcc"

def cc_process(cmd):
    '''cmd'''
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
        raise Exception(err)

def lines_string(name, lines):
    '''lines to string'''
    str = name
    for line in lines:
        str = str + line + "\n"
    return str

def process(file):
    '''process'''
    gcc_lines = cc_process([GCC, "-E", file])
    mcc_lines = cc_process([MCC, "-E", file])
    if len(gcc_lines) != len(mcc_lines):
        str1 = lines_string("gcc_lines:\n", gcc_lines)
        str2 = lines_string("mcc_lines:\n", mcc_lines)
        raise Exception(str1+str2)

    for i in range(len(gcc_lines)):
        gcc_line = gcc_lines[i]
        mcc_line = mcc_lines[i]
        if gcc_line != mcc_line:
            str1 = "gcc_line: " + gcc_line + "\n"
            str2 = "mcc_line: " + mcc_line + "\n"
            raise Exception(str1+str2)

    return lines_string("gcc_lines:\n", gcc_lines), lines_string("mcc_lines:\n", mcc_lines)

def dir_srcs(dir):
    '''get dir srcs'''
    return [os.path.join(dir, f) for f in os.listdir(dir) if os.path.isfile(os.path.join(dir, f)) and f.endswith(".c") and f != "cpp.c"]

def main():
    '''main'''

    dir = os.path.join(os.getcwd(), "test/cpp")
    files = dir_srcs(dir)
    for file in files:
        try:
            glines, mlines = process(file)
            print "%s \033[92mPASS\033[0m" % file
            # print glines
            # print mlines
        except Exception,e:
            print e
            print "%s \033[91mFAIL\033[0m" % file
            sys.exit(1)

if __name__ == "__main__":
    reload(sys)
    sys.setdefaultencoding('utf8')
    sys.exit(main())
