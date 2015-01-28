#!/usr/bin/python
#-*- coding: utf-8 -*-
#
# fsync test

import os
import sys
import subprocess
import tempfile
import re

CC="/usr/bin/c99"
MCC=os.path.abspath("./mcc")
tmpdir = tempfile.mkdtemp()
inc="../src/cc/lib"
tmp1="1.i"
tmp2="2.i"

def process(file, argv=None):
    """process a file"""

    cc_argv = [CC, "-E", file]
    mcc_argv = [MCC, file]

    if argv:
        cc_argv = cc_argv + argv
        mcc_argv = mcc_argv + argv
    
    # cc
    p1 = subprocess.Popen(cc_argv, stdout = subprocess.PIPE)
    p2 = subprocess.Popen(["grep", "^#"], stdin = p1.stdout, stdout = subprocess.PIPE)
    p1.stdout.close()
    out = p2.communicate()[0].strip()
    lines = out.splitlines()
    content = ""
    for line in lines:
        res = re.findall("^#\s*[0-9]+\s*\"(.*)\"", line)
        if len(res) == 0:
            #content = content + line.strip() + "\n"
            pass
        else:
            com = line.split()
            content = content + com[0] + " " + com[1] + " \"" + res[0] + "\"\n"
    f = open(os.path.join(tmpdir, tmp1), "w")
    f.write(content)
    f.close()

    # mcc
    p1 = subprocess.Popen(mcc_argv, stderr = subprocess.PIPE)
    out = p1.communicate()[1]
    f = open(os.path.join(tmpdir, tmp2), "w")
    f.write(out)
    f.close()

    file1 = os.path.join(tmpdir, tmp1)
    file2 = os.path.join(tmpdir, tmp2)
    if subprocess.call(["diff", file1, file2], stdout = open("/dev/null", "w"), stderr = open("/dev/null", "w")) == 0:
        print "[OK] "+file
    else:
        print "[FAILED]", file
        print content
        print "==="
        print out
        sys.exit(1)

def main():
    """main"""
    if len(sys.argv) < 2:
        print "fsync.py <input-file> -I[include directory]"
        sys.exit(1)

    argv = sys.argv
    incs = []
    input_file = None
    for arg in argv[1:]:
        if arg.startswith("-I"):
            incs.append(arg)
        else:
            input_file = arg

    if not input_file:
        print "no input file."
        sys.exit(1)
            
    if os.path.isfile(input_file):
        if input_file.endswith(".c") or input_file.endswith(".h"):
            path = os.path.abspath(os.path.expanduser(input_file))
            process(path, incs)
        else:
            print "Not a c file:", input_file
    elif os.path.isdir(input_file):
        path = os.path.abspath(os.path.expanduser(input_file))
        files = [f for f in os.listdir(path) if os.path.isfile(os.path.join(path, f)) and (f.endswith(".c") or f.endswith(".h"))]
        for f in files:
            file = os.path.join(path, f)
            process(file, incs)
    else:
        print "No such file or directory:", input_file
    
if __name__ == "__main__":
    reload(sys)
    sys.setdefaultencoding('utf8')
    sys.exit(main())
