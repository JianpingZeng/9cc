
### `总进度表`

|  计划点     | 完成情况说明 |
|------------|-------------|
| `预处理`    |  完成    |
| `词法分析`  |  完成    |
| `语法分析`  |         |
| `表达式语法分析` |  完成  |
| `语句语法分析`  |  完成  |
| `声明语法分析`  |  TODO  |
| `语义分析`    |         |
| `后端`       |         |


### `DONE` cpp

   调用外部cpp

   待：选项过滤

   gcc -E 会删除多余空白符，导致列号不准。

   -traditional-cpp 选项可以保留空白和注释。

   即使使用上述选项，gcc 也会在文件末尾添加换行符。

### `DONE` lexer:fsync

   写单元测试，针对fsync函数进行大批量测试
   
   待：#pragma等支持

### `DONE` 完成driver，调用外部预处理器

   Mac OS X 上不能直接调用/usr/bin/cpp，总是会报错，跟 Availability.h 有关。

   应该调用 c99 -E，man c99 可以看到这是一个标准C编译器

   待：as、ld的调用

   完成后删除临时文件

### `DONE` 移除tname，改用register_print_function

    仍保留。


### `DONE` 完成词法分析器

   词法分析器接受的输入来自于预处理器的输出

   遵循UNIX哲学，词法分析器从标准输入(stdin)读入
   
   C预处理器格式

   # 行号 文件名 其他

   每行第一个字符总是 #。例如：

   # 3 "/usr/include/stdio.h" 

   上面的示例表示该行的下面一行是 stdio.h 的第3行

   目前有这三部分足矣，<其他>部分待查

   完成情况统计

   1. 注释：// 和 /###/，完成
   2. 关键词：C99共37个，完成
   3. 操作符和分隔符：完成
   4. 标识符：identifiers，完成
   5. 常量：
   
      5.1 整数常量：完成

        八进制：完成
        十六进制：完成
        十进制：完成

      5.2 字符常量：完成

      5.3 浮点数常量：完成

      5.4 字符串常量：完成

      
   待：字符串常量的数组类型
   

### `DONE` 词法分析器的测试

    写脚本大批量测试：脚本已完成

    使用`lex.py`工具测试`mcc`词法分析器，已测试通过的软件包：

    * diffutils

    * emacs

    * gawk
    
    * glibc
    
    * gzip

    * lcc

    * sed

    * sqlite

    * tar

    * wget

    未通过软件包：

    * bash:

      lib/sh/strtod.c 浮点数溢出


#### `DONE` Bugs

     mcc driver 对 -I 路径进行扩展为绝对路径：完成

     log.v：完成

#### `DONE` 完成内存分配程序

     以及源代码目录修改

     待：alloc已完成，但未测试，包括：

     1. 正确性测试

     2. 性能测试


### `DONE` 完成表达式的语法分析

    完成部分

    待：其他部分

### `DONE` 完成语句的二叉树设计

统一采用二叉树

| 语句        | 节点     | 左子树   |  右子树|
|------------|----------|---------|-------|
| `if`       | IfStmt   |  expr   | stmt  |
| `if-else`  | ElseStmt |  IfStmt | stmt  |
| `while`    | WhileStmt | expr   | stmt  |
| `do-while` | DoWhileStmt | stmt | expr  |
| `for`      | ForStmt  | concat-node | stmt |
| `switch`   | SwitchStmt | expr | stmt   |
| `compound` | CompoundStmt | concat-node | NULL |
| `continue` | ContinueStmt | NULL  | NULL |
| `break`    | BreakStmt  |  NULL   | NULL |
| `return`   | ReturnStmt | expr    | NULL |
| `goto`     | GotoStmt   | expr    | NULL |
| `case`     | CaseStmt   | expr    | stmt |
| `default`  | DefaultStmt | stmt   | NULL |
| `label`    | LabelStmt   | stmt   | NULL |

二叉树简化了设计，但存在一个问题，就是上面的 `for` 和 `compound` 遇到的。

以 `compound` 为例，其结构是多个 `declaration` 和 `statement` 的并列。二叉树的两个子节点不足以表示。`for` 循环的三个表达式也是如此。因此引进了 `concat-node`。

`for` 的 `concat-node` 结构如下：

      	 	       <ForStmt>
            	       |
                 --------------
       		     |            |
  	       <concat-node>     NULL
       	         |
  	       -------------
  	       |           |
  	    <expr1>    <concat-node>
	                   |
	              -------------
  	              |           |
  	           <expr2>    <concat-node>
		                      |
	                     -------------
  	                     |           |
  	                  <expr3>       NULL
  	                  

`compound` 的 `concat-node` 结构如下：

      	 	       <CompoundStmt>
            	       |
                 --------------
       		     |            |
  	       <concat-node>     NULL
       	         |
  	       -------------
  	       |           |
  	<decl1/stmt1>    <concat-node>
	                   |
	              -------------
  	              |           |
  	      <decl2/stmt2>    <concat-node>
		                      |
	                     -------------
  	                     |           |
  	              <decl3/stmt3>     ......


### `DONE` 完成语句的语法分析

1. `case` 和 `default` 只能出现在 `switch` 语句中。

2. `break` 只能出现在 `for`, `while`, `do-while` 和 `switch` 中。

3. `continue` 只能出现在 `for`, `while` 和 `do-while` 中。


### `TODO` 完成声明的语法分析

    1. 设计符号表
    2. 设计types