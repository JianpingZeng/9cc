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

class TestUnit:
    name = ""
    snippets = []

class TestSnippet:
    code = ""
    expect = ""

def die():
    """fail"""
    exit(1)

def run_snippet(snippet):
    """run snippet"""
    print snippet.code
    print snippet.expect

def run_unit(unit):
    """run unit"""
    print "Testing " + unit.name + " ..."
    for snippet in unit.snippets:
        run_snippet(snippet)

def parse_xml(path):
    """process a xml"""
    testUnit = TestUnit()
    
    dom = minidom.parse(path)
    unit = dom.getElementsByTagName("unit")[0]
    snippets = unit.getElementsByTagName("snippet")
    
    testUnit.name = unit.attributes["name"].value
    
    for snippet in snippets:
        testSnippet = TestSnippet()
        code = snippet.getElementsByTagName("code")[0]
        expect = snippet.getElementsByTagName("expect")[0]
        testSnippet.code = code.firstChild.nodeValue
        testSnippet.expect = expect.firstChild.nodeValue;
        testUnit.snippets.append(testSnippet)

    return testUnit

def main():
    """main"""
    print
    dir = os.path.dirname(sys.argv[0])
    xmls = [f for f in os.listdir(dir) if f.endswith(".xml")]
    for xml in xmls:
        path = os.path.join(dir, xml)
        unit = parse_xml(path)
        run_unit(unit)
    
    return 0
    
if __name__ == "__main__":
    reload(sys)
    sys.setdefaultencoding('utf8')
    sys.exit(main())
