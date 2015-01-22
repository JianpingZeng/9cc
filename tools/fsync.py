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

def process(file):
    """process a file"""

    # cc
    p1 = subprocess.Popen([CC, "-E", file, "-I"+inc], stdout = subprocess.PIPE)
    p2 = subprocess.Popen(["grep", "^#"], stdin = p1.stdout, stdout = subprocess.PIPE)
    p1.stdout.close()
    out = p2.communicate()[0].strip()
    lines = out.splitlines()
    content = ""
    for line in lines:
        res = re.findall("^#\s*[0-9]*\s*\"(.*)\"", line)[0]
        com = line.split()
        content = content + com[0] + " " + com[1] + " \"" + res + "\"\n"
    f = open(os.path.join(tmpdir, tmp1), "w")
    f.write(content)
    f.close()

    # mcc
    p1 = subprocess.Popen([MCC, file, "-I"+inc], stderr = subprocess.PIPE)
    out = p1.communicate()[1]
    f = open(os.path.join(tmpdir, tmp2), "w")
    f.write(out)
    f.close()

    if subprocess.call(["diff", os.path.join(tmpdir, tmp1), os.path.join(tmpdir, tmp2)]) == 0:
        print "[OK] "+file
    else:
        print "[FAILED] "+file

def main():
    """main"""
    path = os.path.abspath("../src/cc")
    files = [f for f in os.listdir(path) if os.path.isfile(os.path.join(path, f)) and (f.endswith(".c") or f.endswith(".h"))]
    for f in files:
        file = os.path.join(path, f)
        process(file)
    
if __name__ == "__main__":
    reload(sys)
    sys.setdefaultencoding('utf8')
    sys.exit(main())
