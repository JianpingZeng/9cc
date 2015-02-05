// node

_ns(BEGIN_NODE_ID)

// decl
_ns(BEGIN_DECL_ID)

_n(TRANSLATION_UNIT_DECL,   "TranslationUnitDecl")
_n(VAR_DECL,                "VarDecl")
_n(FUNC_DECL,               "FuncDecl")
_n(PARAMVAL_DECL,           "ParamVarDecl")
_n(TYPEDEF_DECL,            "TypedefDecl")
_n(ENUM_DECL,               "EnumDecl")
_n(STRUCT_DECL,             "StructDecl")
_n(UNION_DECL,              "UnionDecl")

_ns(END_DECL_ID)

// expr
_ns(BEGIN_EXPR_ID)

_n(BINARY_OPERATOR,         "BinaryOperator")
_n(UNARY_OPERATOR,          "UnaryOperator")
_n(ADDR_OPERATOR,           "AddrOperator")

_ns(BEGIN_LITERAL_ID)

_n(INTEGER_LITERAL,         "IntegerLiteral")
_n(FLOAT_LITERAL,           "FloatLiteral")
_n(STRING_LITERAL,          "StringLiteral")

_ns(END_LITERAL_ID)

_ns(END_EXPR_ID)

// stmt
_ns(BEGIN_STMT_ID)

_n(IF_STMT,                 "IfStmt")
_n(WHILE_STMT,              "WhileStmt")
_n(DOWHILE_STMT,            "DoWhileStmt")
_n(COMPOUND_STMT,           "CompoundStmt")
_n(RETURN_STMT,             "ReturnStmt")

_ns(END_STMT_ID)

_ns(END_NODE_ID)

#undef _n
#undef _ns
