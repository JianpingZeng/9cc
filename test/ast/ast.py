#!/usr/bin/python
#-*- coding: utf-8 -*-
#
# ast test

import os
import sys
import subprocess
import tempfile
import re
from xml.dom import minidom

class Node:
    """ast node"""

class Decl(Node):
    """decl"""

class Stmt(Node):
    """stmt"""

class Expr(Node):
    """expr"""

class TestRoot:
    name = ""
    units = []

class TestUnit:
    code = ""
    exts = []

def die():
    """fail"""
    exit(1)

def run_code(code):
    """compile code and return the output"""
    mcc = "./mcc"
    ifile = tempfile.mktemp(".c")
    fp = open(ifile, "w")
    fp.write(code+"\n")
    fp.close()

    # call mcc
    command = [mcc, ifile, "-o null"]
    p = subprocess.Popen(command, stderr = subprocess.PIPE, stdout = subprocess.PIPE)
    out, err = p.communicate()
    print "out: " + out
    print "err: " + err
    print ifile

def run_unit(unit):
    """run snippet"""
    run_code(unit.code)

def run_root(root):
    """run unit"""
    print "Testing " + root.name + " ..."
    for unit in root.units:
        run_unit(unit)

def parse_expect(expect):
    """parse xml's expect nodes to ast nodes"""
    exts = []
    return exts

def parse_xml(path):
    """process a xml"""
    testRoot = TestRoot()
    
    dom = minidom.parse(path)
    root = dom.getElementsByTagName("root")[0]
    units = root.getElementsByTagName("unit")
    
    testRoot.name = root.attributes["name"].value
    
    for unit in units:
        testUnit = TestUnit()
        code = unit.getElementsByTagName("code")[0]
        expect = unit.getElementsByTagName("expect")[0]
        testUnit.code = code.firstChild.nodeValue
        testUnit.exts = expect;
        testRoot.units.append(testUnit)

    return testRoot

def main():
    """main"""
    print
    dir = os.path.dirname(sys.argv[0])
    xmls = [f for f in os.listdir(dir) if f.endswith(".xml")]
    for xml in xmls:
        path = os.path.join(dir, xml)
        root = parse_xml(path)
        run_root(root)
    
    return 0
    
if __name__ == "__main__":
    reload(sys)
    sys.setdefaultencoding('utf8')
    sys.exit(main())
