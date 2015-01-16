// node

_ns(NODE_ID_BEGIN)

// decl
_ns(DECL_ID_BEGIN)
_n(TRANSLATION_UNIT_DECL, "TranslationUnitDecl")
_n(VAR_DECL,    "VarDecl")
_n(FUNC_DECL,   "FuncDecl")
_n(PARAMVAL_DECL, "ParamVarDecl")
_n(TYPEDEF_DECL, "TypedefDecl")
_n(ENUM_DECL, "EnumDecl")
_n(STRUCT_DECL, "StructDecl")
_n(UNION_DECL, "UnionDecl")
_ns(DECL_ID_END)

// expr
_ns(EXPR_ID_BEGIN)
_n(BINARY_OPERTAOR, "BinaryOperator")
_n(UNARY_OPERATOR, "UnaryOperator")
_n(ADDR_OPERATOR, "AddrOperator")

_ns(LITERAL_ID_BEGIN)
_n(INTEGER_LITERAL, "IntegerLiteral")
_n(FLOAT_LITERAL, "FloatLiteral")
_n(STRING_LITERAL, "StringLiteral")
_ns(LITERAL_ID_END)
_ns(EXPR_ID_END)

// stmt
_ns(STMT_ID_BEGIN)
_n(IF_STMT,         "IfStmt")
_n(WHILE_STMT,      "WhileStmt")
_n(DOWHILE_STMT,    "DoWhileStmt")
_n(COMPOUND_STMT,   "CompoundStmt")
_n(RETURN_STMT,     "ReturnStmt")
_ns(STMT_ID_END)

_ns(NODE_ID_END)

#undef _n
#undef _ns
