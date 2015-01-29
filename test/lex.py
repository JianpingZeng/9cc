#!/usr/bin/python
#-*- coding: utf-8 -*-
#
#

import os
import sys
import subprocess
import tempfile
import re

GCC="/usr/bin/gcc"
MCC=os.path.abspath("./mcc")

def gcc_process(file, argv=None):
    '''process a file'''

    gcc_argv = [GCC, "-E", file]

    if argv:
        gcc_argv = gcc_argv + argv

    p = subprocess.Popen(gcc_argv, stdout=subprocess.PIPE)
    out = p.communicate()[0]
    lines = out.splitlines()
    


def mcc_process(file, argv=None):
    '''process a file'''

    mcc_argv = [MCC, file]

    if argv:
        mcc_argv = mcc_argv + argv

    
    

def main():
    '''main'''
    

if __name__ == "__main__":
    reload(sys)
    sys.setdefaultencoding('utf8')
    sys.exit(main())
