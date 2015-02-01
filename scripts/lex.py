#!/usr/bin/python
#-*- coding: utf-8 -*-
#
#

import os
import sys
import subprocess
import tempfile
import re
import tempfile


TMPDIR = tempfile.mkdtemp()
GCC = "/usr/bin/gcc"
MCC = os.path.abspath("./mcc")
gcc_i = os.path.join(TMPDIR, "1.i")
mcc_i = os.path.join(TMPDIR, "2.i")

class Line(object):
    def __init__(self, cstr):
        self.cstr = cstr

    def __getitem__(self, key):
        if isinstance(key, slice):
            # start stop step
            return self.cstr[key.start:key.stop]
        elif isinstance(key, int):
            if key >= len(self.cstr):
                return ""
            else:
                return self.cstr[key]
        else:
            raise TypeError, "invalid type"

    def startswith(self, key):
        return self.cstr.startswith(key)

    def __len__(self):
        return len(self.cstr)

    def __str__(self):
        return self.cstr

def issep(ch):
    sep = ['(', ')', '{', '}', '[', ']', ',', ';', ':', '~', '?']
    for k in sep:
        if ch == k:
            return True
    return False

def isdigitletter(ch):
    return ch.isdigit() or ch.isalpha() or ch == '_'

def ishex(ch):
    hex = ['a', 'b', 'c', 'd', 'e', 'f',
           'A', 'B', 'C', 'D', 'E', 'F']
    for h in hex:
        if ch == h:
            return True
    return False

def isdigithex(ch):
    return ch.isdigit() or ishex(ch)

def iskeyword(line, i):
    kw = ['auto',
          'break',
          'case', 'char', 'const', 'continue',
          'default', 'do', 'double',
          'extern', 'else', 'enum',
          'float', 'for',
          'goto',
          'int', 'if', 'inline',
          'long',
          'return', 'restrict', 'register',
          'static', 'short', 'sizeof', 'switch', 'struct', 'signed',
          'typedef',
          'unsigned', 'union',
          'void', 'volatile',
          'while',
          '_Bool', '_Complex', '_Imaginary']
    for k in kw:
        if line[i:].startswith(k):
            s = line[i+len(k)]
            if not isdigitletter(s):
                return True, k
    return False, ""

def isblank(ch):
    space = [' ', '\t', '\v', '\f']
    for c in space:
        if ch == c:
            return True
    return False

def isopeq(ch):
    op = ['/', '+', '-', '*', '=', '!', '%', '^', '&', '|']
    for o in op:
        if ch == o:
            return True
    return False

tokens = 0
errors = 0
warnings = 0
gcc_out = ""

def puttok(token):
    '''put token'''
    global gcc_out
    global tokens
    gcc_out = gcc_out + token + "\n"
    tokens = tokens + 1

def error(key):
    sys.stderr.write(key+"\n")
    global errors
    errors = errors + 1

def inumber(line, beg, i):
    '''integer'''
    ull = '(u|U)(ll|LL)'
    llu = '(ll|LL)(u|U)'
    ll = '(ll|LL)'
    lu = '(l|L)(u|U)'
    ul = '(u|U)(l|L)'

    if re.search(ull, line[i:i+3]) or re.search(ull, line[i:i+3]):
        i = i+3
    elif re.search(ll, line[i:i+2]):
        i = i+2
    elif re.search(lu, line[i:i+2]) or re.search(ul, line[i:i+2]):
        i = i+2
    elif line[i] == 'l' or line[i] == 'L':
        i = i+1
    elif line[i] == 'u' or line[i] == 'U':
        i = i+1
    else:
        pass
    
    return i-beg

def number(line, i):
    '''number'''
    beg = i
    if line[i] == '0' and (line[i+1] == 'x' or line[i+1] == 'X'):
        # hex
        i = i+2
        if not line[i].isdigit() and not line[i] != '.':
            error("incomplete hex constant")
            return inumber(line, beg, i)

        while isdigithex(line[i]):
            i = i+1

        if line[i] == '.' or line[i] == 'p' or line[i] == 'P':
            # float constant
            return fnumber(line, beg, i, 16)
        else:
            return inumber(line, beg, i)
                
    elif line[i] == '0':
        # octal
        while line[i].isdigit():
            if line[i] == '8' or line[i] == '9':
                error("invalid octal constant")
            i = i+1

        if line[i] == '.' or line[i] == 'e' or line[i] == 'E':
            # float constant
            return fnumber(line, beg, i, 10)
        else:
            return inumber(line, beg, i)
        
    else:
        # dec
        while line[i].isdigit():
            i = i+1

        if line[i] == '.' or line[i] == 'e' or line[i] == 'E':
            # float constant
            return fnumber(line, beg, i, 10)
        else:
            return inumber(line, beg, i)
        

def fnumber(line , b, f, base):
    '''fnumber'''
    i = f
    if base == 10:
        if line[i] == '.':
            i = i+1
            while line[i].isdigit():
                i = i+1

        if line[i] == 'e' or line[i] == 'E':
            i = i+1
            if line[i] == '+' or line[i] == '-':
                i = i+1

            if line[i].isdigit():

                while line[i].isdigit():
                    i = i+1
                
            else:
                error("exponent used with no following digits")
                
    else:
        if line[i] == '.':
            i = i+1
            if not isdigithex(line[i-2]) and not isdigithex(line[i]):
                error("hex floating constants requires a significand")

            while isdigithex(line[i]):
                i = i+1

        if line[i] == 'p' or line[i] == 'P':
            i = i+1
            if line[i] == '+' or line[i] == '-':
                i = i+1
                if line[i].isdigit():
                    while line[i].isdigit():
                        i = i+1
                else:
                    error("exponent has no digits")
        else:
            error("hex floating constants require an exponent")
                    


    # handle suffix
    suffix = ['f', 'F', 'l', 'L']
    for s in suffix:
        if s == line[i]:
            i = i+1
            break

    return i-b
    

def parse_line(line):
    '''parse a line'''
    i = 0
    end = len(line)
    while True:
        while isblank(line[i]):
            i = i+1
        if i >= end:
            break
        iskw, kw = iskeyword(line, i)
        
        if iskw:
            # keyword
            i = i + len(kw)
            puttok(kw)
            
        elif (line[i] == 'L' and line[i+1] == '"') or line[i] == '"':
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
                error("unterminated string constant")
            else:
                i = i+1
                puttok(line[beg:i])
                    
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
                error("unterminated character constant")
            else:
                i = i+1
                puttok(line[beg:i])
        
        elif line[i].isalpha() or line[i] == '_':
            # identifier
            beg = i
            while isdigitletter(line[i]):
                i = i+1
            puttok(line[beg:i])
            
        elif line[i].isdigit():
            # number
            step = number(line, i)
            puttok(line[i:i+step])
            i = i+step

        elif line[i] == '.':
            if line[i+1] == '.' and line[i+2] == '.':
                puttok(line[i:i+3])
                i = i+3
            elif line[i+1].isdigit():
                # float constant
                step = fnumber(line, i, i, 10)
                puttok(line[i:i+step])
                i = i+step
            else:
                puttok(line[i:i+1])
                i = i+1

        elif issep(line[i]):
            # separator
            puttok(line[i])
            i = i+1

        elif isopeq(line[i]):
            # operator
            if line[i+1] == '=':
                puttok(line[i:i+2])
                i = i+2
            elif line[i] == '+' and line[i+1] == '+':
                puttok(line[i:i+2])
                i = i+2
            elif line[i] == '-' and line[i+1] == '-':
                puttok(line[i:i+2])
                i = i+2
            elif line[i] == '-' and line[i+1] == '>':
                puttok(line[i:i+2])
                i = i+2
            elif line[i] == '&' and line[i+1] == '&':
                puttok(line[i:i+2])
                i = i+2
            elif line[i] == '|' and line[i+1] == '|':
                puttok(line[i:i+2])
                i = i+2
            else:
                puttok(line[i:i+1])
                i = i+1

        elif line[i] == '<':
            if line[i+1] == '<' and line[i+2] == '=':
                puttok(line[i:i+3])
                i = i+3
            elif line[i+1] == '<' or line[i+1] == '=':
                puttok(line[i:i+2])
                i = i+2
            else:
                puttok(line[i:i+1])
                i = i+1
            
        elif line[i] == '>':
            if line[i+1] == '>' and line[i+2] == '=':
                puttok(line[i:i+3])
                i = i+3
            elif line[i+1] == '>' or line[i+1] == '=':
                puttok(line[i:i+2])
                i = i+2
            else:
                puttok(line[i:i+1])
                i = i+1
                
        else:
            error("invalid character: "+line[i]+"\n")
            i = i+1
    
    

def gcc_process(file, argv=None):
    '''process a file'''

    gcc_argv = [GCC, "-E", file]

    if argv:
        gcc_argv = gcc_argv + argv

    p = subprocess.Popen(gcc_argv, stdout=subprocess.PIPE, stderr=open("/dev/null", "w"))
    out= p.communicate()[0]
    lines = out.splitlines()
    
    if p.poll() == 0:
        global tokens
        global errors
        global warnings
        tokens = 0
        errors = 0

        global gcc_out
        gcc_out = ""
        for line in lines:
            line = line.strip()
            if not line.startswith("#") and len(line) > 0:
                parse_line(Line(line))

        sumary = "%u tokens, %u errors, %u warnings\n" % (tokens, errors, warnings)
        gcc_out = gcc_out + sumary
    
        f = open(gcc_i, "w")
        f.seek(0)
        f.write(gcc_out)
        f.close()

        return gcc_i

    else:
        return None
                

def mcc_process(file, argv=None):
    '''process a file'''

    mcc_argv = [MCC, file]

    if argv:
        mcc_argv = mcc_argv + argv

    p = subprocess.Popen(mcc_argv, stderr=subprocess.PIPE)
    out = p.communicate()[1]

    if p.poll() == 0:
        f = open(mcc_i, "w")
        f.seek(0)
        f.write(out)

        return mcc_i
    else:
        return None

oks = 0
skips = 0

def process(file, argv=None):
    '''process'''

    global oks
    global skips
    
    file1 = gcc_process(file, argv)
    file2 = mcc_process(file, argv)

    if file1 and file2:
        # diff
        ret = subprocess.call(["diff", file1, file2], stderr=open("/dev/null", "w")) 
        if ret == 0:
            print "[OK] "+file
            oks = oks + 1
        else:
            print "[FAILED]", file
            sys.exit(1)
    else:
        print "[SKIPPED]", file, "<preprocessor failed>"
        skips = skips + 1

def process_dir(dir, argv=None):
    '''process a dir'''

    path = dir
    files = [f for f in os.listdir(path) if os.path.isfile(os.path.join(path, f)) and (f.endswith(".c"))]
    for f in files:
        file = os.path.join(path, f)
        process(file, argv)

    dirs = [f for f in os.listdir(path) if os.path.isdir(os.path.join(path, f))]
    for d in dirs:
        subdir = os.path.join(path, d)
        process_dir(subdir, argv)
        
def pre_process():
    '''pre'''


def post_process():
    '''post'''
    print oks, "[OK],", skips, "[SKIPPED]"

def main():
    '''main'''
    if len(sys.argv) < 2:
        print sys.argv[0], "<input-file> -I[include directory]"
        sys.exit(1)

    argv = sys.argv
    incs = []
    input_file = None
    for arg in argv[1:]:
        if arg.startswith("-I"):
            p = os.path.expanduser(arg[2:])
            incs.append("-I"+p)
        else:
            input_file = arg

    if not input_file:
        print "no input file."
        sys.exit(1)

    pre_process()
            
    if os.path.isfile(input_file):
        if input_file.endswith(".c"):
            path = os.path.abspath(os.path.expanduser(input_file))
            process(path, incs)
        else:
            print "Not a c file:", input_file
    elif os.path.isdir(input_file):
        path = os.path.abspath(os.path.expanduser(input_file))
        process_dir(path, incs)
    else:
        print "No such file or directory:", input_file


    post_process()

if __name__ == "__main__":
    reload(sys)
    sys.setdefaultencoding('utf8')
    sys.exit(main())
