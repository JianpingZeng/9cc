#!/usr/bin/python
#-*- coding: utf-8 -*-
#
# ast test

import os
import sys
import subprocess
import tempfile
import re

def main():
    """main"""
    print
    for arg in sys.argv:
        print arg
    
    return 0
    
if __name__ == "__main__":
    reload(sys)
    sys.setdefaultencoding('utf8')
    sys.exit(main())
