
### `总进度表`

|  计划点     | 完成情况说明 |
|------------|-------------|
| `预处理`    |  完成（暂时调用第三方）    |
| `词法分析`  |  完成    |
| `语法分析`  |    ing...    |
| `语义分析`    |   ing...      |
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
| `for`      | ForStmt  | anode | stmt |
| `switch`   | SwitchStmt | expr | stmt   |
| `compound` | CompoundStmt | anode | NULL |
| `continue` | ContinueStmt | NULL  | NULL |
| `break`    | BreakStmt  |  NULL   | NULL |
| `return`   | ReturnStmt | expr    | NULL |
| `goto`     | GotoStmt   | expr    | NULL |
| `case`     | CaseStmt   | expr    | stmt |
| `default`  | DefaultStmt | stmt   | NULL |
| `label`    | LabelStmt   | stmt   | NULL |

二叉树简化了设计，但存在一个问题，就是上面的 `for` 和 `compound` 遇到的。

以 `compound` 为例，其结构是多个 `declaration` 和 `statement` 的并列。二叉树的两个子节点不足以表示。`for` 循环的三个表达式也是如此。

最初使用了 `vector`，但觉得破坏了二叉树结构，比如为了 `for` statement， `stmt` 要增加一个 `union node **exprs` 的字段，如果仅仅 `for` 循环用到了这个字段，显得多余。

因此引进了 `concat-node`，保持二叉树结构，但一旦列表的项目增多，冗余的 `concat-node` 太多，而且不便于遍历操作。

于是又改回了 `vector`，但有改进，新增了 `anode`，其有一个 `union node **` 字段保存节点列表，但其本身又是一个 `node`，因此，既没有冗余节点，二叉树的结构仍然得以保持。


### `DONE` 完成语句的语法分析

1. `case` 和 `default` 只能出现在 `switch` 语句中。

2. `break` 只能出现在 `for`, `while`, `do-while` 和 `switch` 中。

3. `continue` 只能出现在 `for`, `while` 和 `do-while` 中。


### `TODO` 完成声明的类型设计

1. enum type

2. struct/union type

3. typedef type

		op: TYPEDEF
		name: typedef name
		type: typedef-ed type

4. function type

		op: FUNCTION
		type: return type
		u.f.proto: (parameter type list) 
		
5. pointer type

6. identical types (integer, floating)


### `TODO` 完成声明的语法分析

    1. 设计符号表
    2. 设计types
    
    
### branch `refactor1`

1. 把`type`从lexer解耦，lexer只返回原始的token字符串，解析移至parser，例如，10只返回字符串"10"和ICONSTANT，至于其type,value,这些移到parser解析。

2. 把`symbol`从type解耦，目前type指向symbol，symbol又指向type，比较混乱，改成单向的。

3. `expr`解析构造时，及时填入信息，比如type，这样可以大大简化eval,lvalue函数。

### `TODO` Reactor `mcc`

mcc 作为一个命令行驱动程序，其结构如下：
	
	Parent Process              Child Process
	   0. mcc  ---------------  1. cpp_main  (preprocess)
		                        2. cc_main   (compile)
		                        3. as        (assemble)
       4. ld (link)
       
步骤 1,2,3 称为`translate`，设输入源文件个数为N，则`translate`将跑N次。

**要求**

1. mcc 在这N次中，不能有内存泄露。
2. 简化 mcc 的代码，固定其结构。
