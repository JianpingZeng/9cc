# mcc
A standard c compiler. 

_NOT_ finished yet. See [TODO.md](TODO.md)

# Notes
The compiler is designed to be able to compile itself, so it is written in C. And it refuses to use any gnu extension, _ONLY_ pure standard syntax. It is intended to support all C99 language features while keeping the code as simple and small as possible.

# Build
To build the mcc compiler, run command:

  	make

To run the testing suite, run command:

    make test

# Author
Guiyang Huang, [mohu3g@163.com](mailto:mohu3g@163.com)

# Acknowledgement
The data structures of `type` and `symbol table` are inspired by lcc.

# License
mcc is free software published under GNU GPLv2.

# Reference

* `C: A Reference Manual` by Samuel P. Harbison III, Guy L. Steele Jr.

* `lcc`: [https://github.com/drh/lcc](https://github.com/drh/lcc)

* `C99 standard final draft` [http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf)

* `The x86-64 ABI` [http://www.x86-64.org/documentation/abi.pdf](http://www.x86-64.org/documentation/abi.pdf)
]
