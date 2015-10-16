#!/usr/bin/python
#-*- coding: utf-8 -*-
#

import os
import sys
import subprocess
import tempfile
import re

GCC = "/usr/bin/gcc"
# GCC = os.path.expanduser("~/Downloads/8cc/8cc")
MCC = "./mcc"
# SDK_PATH = subprocess.check_output(["xcrun", "--show-sdk-path"]).strip()
# SDK_INC = SDK_PATH + "/usr/include"
SDK_INC = "/usr/include"

def get_incs():
    '''get include dirs'''
    dirs = [SDK_INC, "utils", "sys", "/usr/include/x86_64-linux-gnu"]
    ret = []
    for dir in dirs:
        ret.append("-I" + dir)
    return ret

def do_process(cmd):
    '''cmd'''
    p = subprocess.Popen(cmd, stdout = subprocess.PIPE, stderr = subprocess.PIPE)
    out, err = p.communicate()
    lines = out.splitlines()
    ret = []

    if p.poll() == 0:
        '''ok'''
        prog = os.path.basename(cmd[0])
        if prog != "mcc":
            f = open("./0.i", "w")
            for line in lines:
                f.write(line+"\n")
            f.close()
            
        for line in lines:
            line = line.strip()
            if len(line) == 0 or line.startswith('#'):
                continue
            ret.append(line)
        return ret
    else:
        '''fail'''
        print err
        sys.exit(1)

def gcc_process(file, argv=None):
    '''gcc'''
    gcc_argv = [GCC, "-E", "-U__GNUC__", "-U__PTRDIFF_TYPE__", "-U__SIZE_TYPE__", "-U__WCHAR_TYPE__", "-U__WINT_TYPE__"]

    if argv:
        gcc_argv = gcc_argv + argv
    
    gcc_argv.append(file)

    return do_process(gcc_argv)

def mcc_process(file, argv=None):
    '''mcc'''
    mcc_argv = [MCC, "-E", "-I~/Downloads/8cc/include"]

    if argv:
        mcc_argv = mcc_argv + argv
    
    mcc_argv.append(file)
    
    return do_process(mcc_argv)

def isblank(ch):
    space = [' ', '\t', '\v', '\f']
    return ch in space

def issep(ch):
    sep = ['(', ')', '{', '}', '[', ']', ',', ';', ':', '~', '?']
    return ch in sep

def isdigitletter(ch):
    return ch.isdigit() or ch.isalpha() or ch == '_'

def ishex(ch):
    hex = ['a', 'b', 'c', 'd', 'e', 'f',
           'A', 'B', 'C', 'D', 'E', 'F']
    return ch in hex

def isdigithex(ch):
    return ch.isdigit() or ishex(ch)

def isopeq(ch):
    op = ['/', '+', '-', '*', '=', '!', '%', '^', '&', '|']
    return ch in op

class Token:

    def __init__(self, name, line):
        self.name = name
        self.line = line

    def __str__(self):
        return self.name

def line_to_tokens(prog, file, line):
    '''line to tokens'''
    i = 0
    end = len(line)
    line = line + "\n"
    ret = []
    while True:
        if i >= end:
            break;
        while isblank(line[i]):
            i = i + 1
        if i >= end:
            break;

        if (line[i] == 'L' and line[i+1] == '"') or line[i] == '"':
            # string constant
            beg = i
            i = i+1 if line[i] == '"' else i+2
            while line[i] != '"':
                if i >= end:
                    break
                if line[i] == '\\':
                    i = i+2
                else:
                    i = i+1

            if line[i] != '"':
                raise Exception("%s:%s: unterminated string constant [%s] at line [%s]" %
                         (prog, file, line[beg:i], line))
            else:
                i = i+1
                ret.append(Token(line[beg:i], line))
                    
        elif (line[i] == 'L' and line[i+1] == '\'') or line[i] == '\'':
            # character constant
            beg = i
            i = i+1 if line[i] == '\'' else i+2
            while line[i] != '\'':
                if i >= end:
                    break
                if line[i] == '\\':
                    i = i+2
                else:
                    i = i+1

            if line[i] != '\'':
                raise Exception("%s:%s: unterminated character constant [%s] at line [%s]" %
                         (prog, file, line[beg:i], line))
            else:
                i = i+1
                ret.append(Token(line[beg:i], line))
                    
        elif line[i].isalpha() or line[i] == '_':
            # identifier
            beg = i
            while isdigitletter(line[i]):
                i = i+1
            ret.append(Token(line[beg:i], line))

        elif line[i].isdigit():
            # digit
            beg = i
            while line[i].isdigit():
                i = i+1
            ret.append(Token(line[beg:i], line))
            
        else:
            beg = i
            i = i+1
            ret.append(Token(line[beg:i], line))
            
    return ret

def break_to_tokens(prog, file, lines):
    '''convert to tokens'''
    tokens = []
    for line in lines:
        toks = line_to_tokens(prog, file, line)
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
        name1 = tokens1[i].name
        name2 = tokens2[i].name
        if name1 != name2:
            str1 = "%s: [%s] != [%s]\n" % (file, name1, name2)
            str2 = "[%s]\n" % tokens1[i].line
            str3 = "[%s]\n" % tokens2[i].line
            if not name1.isdigit() or not name2.isdigit():
                raise Exception(str1+str2+str3)

        
def process(file, argv=None):
    '''process'''    
    lines1 = gcc_process(file, argv)
    lines2 = mcc_process(file, argv)

    f1 = open("./1.i", "w")
    for line in lines1:
        f1.write(line+"\n")
    f1.close()

    f2 = open("./2.i", "w")
    for line in lines2:
        f2.write(line+"\n")
    f2.close()

    tokens1 = break_to_tokens(os.path.basename(GCC), file, lines1)
    tokens2 = break_to_tokens(os.path.basename(MCC), file, lines2)
    compare(file, tokens1, tokens2)

def dir_srcs(dir):
    '''get dir srcs'''
    return [os.path.join(dir, f) for f in os.listdir(dir) if os.path.isfile(os.path.join(dir, f)) and (f.endswith(".c") or f.endswith(".h"))]

def main():
    '''main'''
    incs = get_incs()
    incs = incs + ["-DCONFIG_X64"]
    files = dir_srcs(os.getcwd())
    # files = ["./alloc.c"]
    print "%d files" % len(files)
    for file in files:
        try:
            process(file, incs)
            print "%s [PASS]" % file
        except Exception,e:
            print e
            print "%s [FAIL]" % file
            # sys.exit(1)

if __name__ == "__main__":
    reload(sys)
    sys.setdefaultencoding('utf8')
    sys.exit(main())
