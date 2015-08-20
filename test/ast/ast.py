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
    name = ""
    attributes = {}
    kids = []
    
    def append(self, k):
        print("(%s %d)append" % (self.name, id(self)))
        self.kids.append(k)

    def __str__(self):
        s = "<" + self.name + " "
        for (k,v) in self.attributes.items():
            s = s + k + "=" + v + " "
        s = s + ">"
        return s

class TestRoot:
    file = ""
    name = ""
    units = []

class TestUnit:
    file = ""
    name = ""
    i = 0
    code = ""
    expect = None

    def __str__(self):
        return "%s: unit %d %s" % (self.file, self.i, self.name)

def die(unit):
    """fail"""
    print("\033[1;31mFAILED\033[0m")
    print(unit)
    exit(1)

def ok():
    """print ok"""
    print("\033[32mOK\033[0m")

def construct(node):
    tree = Node()
    tree.name = node.nodeName
    for (k,v) in node.attributes.items():
        tree.attributes[k] = v
    
    names = ["decl", "expr", "stmt"]
    s = [f for f in node.childNodes if f.nodeName in names]
    for child in s:
        tree.append(construct(child))

    return tree

def construct_tree_from_raw(raw):
    """construct tree from raw output"""

def construct_tree_from_xml(xml):
    """construct tree from raw output"""
    print("\n")
    root = construct(xml)
    print(root.name)
    for k in root.kids:
        print("%s: %d" % (k, id(k)))

def diff(tree, expect):
    """diff two trees"""
    ok()

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
    ret = p.returncode
    if ret != 0:
        die()

    return err

def parse_expect(expect):
    """parse xml's expect nodes to ast nodes"""
    return construct_tree_from_xml(expect)

def parse_xml(path):
    """process a xml"""
    testRoot = TestRoot()
    
    dom = minidom.parse(path)
    root = dom.getElementsByTagName("root")[0]
    units = root.getElementsByTagName("unit")
    
    testRoot.file = path
    if "name" in root.attributes:
        testRoot.name = root.attributes["name"].value
    else:
        testRoot.name = os.path.basename(path).rstrip(".xml")

    for unit in units:
        testUnit = TestUnit()
        code = unit.getElementsByTagName("code")[0]
        expect = unit.getElementsByTagName("expect")[0]
        testUnit.file = path
        if "name" in unit.attributes:
            testUnit.name = unit.attributes["name"].value
        testUnit.i = units.index(unit) + 1
        testUnit.code = code.firstChild.nodeValue
        testUnit.expect = parse_expect(expect)
        testRoot.units.append(testUnit)

    return testRoot

def run_unit(unit):
    """run snippet"""
    raw = run_code(unit.code)
    tree = construct_tree_from_raw(raw)
    diff(tree, unit.expect)

def run_root(root):
    """run unit"""
    sys.stdout.write("Testing " + root.name + " ...")
    for unit in root.units:
        run_unit(unit)

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
    sys.exit(main())
