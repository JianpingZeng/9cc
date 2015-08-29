##C99 Standard Syntax


####External definitions
***

translation-unit:
    
    external-declaration
    translation-unit    external-declaration
    
external-declaration:
    
    declaration
    function-definition
    
function-definition:
   
    declaration-specifier  declarator  declaration-list(opt)  compound-statement
    
declaration-list:

    declaration
    declaration-list	declaration
    
    

####Declaration
***

declaration:
    
    declaration-specifier   init-declarator-list(opt) ;
    
declaration-specifier:
    
    storage-class-specifier     declaration-specifiers(opt)
    type-specifier              declaration-specifiers(opt)
    type-qualifier              declaration-specifiers(opt)
    function-specifier          declaration-specifiers(opt)
    
init-declarator-list:
    
    init-declarator
    init-declarator-list  ,  init-declarator
    
init-declarator:
    
    declarator
    declarator  =  initializer
    
storage-class-specifier: one of
    
    auto    
    extern  
    register    
    static  
    typedef
    
type-qualifier:
    
    const   
    volatile    
    restrict

function-specifier:
    
    inline
    
type-specifier:
    
    void
    char
    short
    int
    long
    float
    double
    signed
    unsigned
    _Bool
    _Complex
    _Imaginary
    enum-specifier
    struct-or-union-specifier
    typedef-name
    
struct-or-union-specifier:
    
    struct-or-union identifier(opt) {  struct-declaration-list  }
    struct-or-union identifier
    
struct-or-union:
    
    struct
    union
    
struct-declaration-list:
    
    struct-declaration
    struct-declaration-list  struct-declaration
    
struct-declaration:
    
    specifier-qulifier-list  struct-declarator-list  ;
    
specifier-qualifier-list:
    
    type-specifier   specifier-qulifier-list(opt)
    type-qualifier   specifier-qulifier-list(opt)
    
struct-declarator-list:
    
    struct-declarator
    struct-declarator-list  ,  struct-declarator
    
struct-declarator:
    
    declarator
    declarator(opt)  :  constant-expression

enum-specifier:
    
    enum  identifier(opt)  {  enumerator-list  }
    enum  identifier(opt)  {  enumerator-list  ,  }
    enum  identifier
    
enumerator-list:
    
    enumerator
    enumerator-list  ,  enumerator
    
enumerator:
    
    enumeration-constant
    enumeration-constant  =  constant-expression
    
enumeration-constant:
    
    identifier
    
typedef-name:
    
    identifier

type-name:
    
    specifier-qualifier-list  abstract-declarator(opt)

declarator:
    
    pointer(opt)  direct-declarator
    
direct-declarator:
    
    identifier
    ( declarator )
    direct-declarator [ type-qualifier-list(opt)  assignment-expression(opt) ]
    direct-declarator [ static  type-qualifier-list(opt)  assignment-expression ]
    direct-declarator [ type-qualifier-list  static  assignment-expression ]
    direct-declarator [ type-qualifier-list(opt) * ]
    direct-declarator ( parameter-type-list )
    direct-declarator ( identifier-list(opt) )

abstract-declarator:
    
    pointer
    pointer(opt)  direct-abstract-declarator
    
direct-abstract-declarator:
    
    (  abstract-declarator  )
    direct-abstract-declarator(opt)  [  assignment-expression(opt)  ]
    direct-abstract-declarator(opt)  [  *  ]
    direct-abstract-declarator(opt)  ( parameter-type-list(opt)  )
    
pointer:
    
    *   type-qualifier-list(opt)
    *   type-qualifier-list(opt)    pointer
    
type-qualifier-list:
    
    type-qualifier
    type-qualifier-list     type-qualifier

parameter-type-list:
    
    parameter-list
    parameter-list  ,  ...
    
parameter-list:
    
    paramater-declaration
    parameter-list  ,  parameter-declaration
    
parameter-declaration:
    
    declaration-specifier  declarator
    declaration-specifier  abstract-declarator(opt)
    
identifier-list:
    
    identifier
    identifier-list  ,  identifier

initializer:
    
    assignment-expression
    {  initializer-list  }
    {  initializer-list  ,  }
    
initializer-list:
    
    designation(opt)  initializer
    initializer-list  ,  designation(opt)  initializer

designation:
    
    designator-list  =
    
designator-list:
    
    designator
    designator-list  designator
    
designator:
    
    [  constant-expression  ]
    .  identifier



####Statement
***

statement:
    
    labeled-statement
    compound-statement
    expression-statement
    selection-statement
    iteration-statement
    jump-statement
    
labeled-statement:
    
    identifier  :  statement
    case  constant-expression  :  statement
    default  :  statement

compound-statement:
    
    {  block-item-list(opt)  }
    
block-item-list:
    
    block-item
    block-item-list  block-item
    
block-item:
    
    declaration
    statement
 
expression-statement:
    
    expression(opt)  ;
    
selection-statement:
    
    if  (  expression  )  statement
    if  (  expression  )  statement  else  statement
    switch  (  expression  )  statement
    
iteration-statement:
    
    while  (  expression  )  statement
    do  statement  while  (  expression  )  ;
    for  ( expression(opt)  ;  expression(opt)  ;  expression(opt)  )  statement
    for  ( declaration  expression(opt)  ;  expression(opt)  )  statement
  
jump-statement:
    
    goto  identifier  ;
    continue  ;
    break  ;
    return  expression(opt)  ;
    
    

####Expression
***

expression:
    
    assignment-expression
    expression ,  assignment-expression
    
assignment-expression:
    
    conditional-expression
    unary-expression  assignment-operator assignment-expression
    
assignment-operator: one of
    
	=  *=  /=  %=  +=  -=  <<=  >>=  &=  ^=  |=

conditional-expression:
    
    logical-OR-expression
    logical-OR-expression ? expression : conditional-expression
    
logical-OR-expression:
    
    logical-AND-expression
    logical-OR-expression || logical-AND-expression

logical-AND-expression:
    
    inclusive-OR-expression
    logical-AND-expression && inclusive-OR-expression

inclusive-OR-expression: 
    
    exclusive-OR-expression
    inclusive-OR-expression | exclusive-OR-expression

exclusive-OR-expression:
    
    AND-expression
    exclusive-OR-expression ^ AND-expression
    
AND-expression:
    
    equality-expression
    AND-expression & equality-expression
    
equality-expression:
    
    relational-expression
    equality-expression == relational-expression
    equality-expression != relational-expression
    
relational-expression:
    
    shift-expression
    relational-expression  <   shift-expression
    relational-expression  >   shift-expression
    relational-expression  <=  shift-expression
    relational-expression  >=  shift-expression

shift-expression:
    
    additive-expression
    shift-expression  <<  additive-expression
    shift-expression  >>  additive-expression
    
additive-expression:
    
    multiplicative-expression
    additive-expression  +  multiplicative-expression
    additive-expression  -  multiplicative-expression
    
multiplicative-expression:
    
    cast-expression
    multiplicative-expression  *  cast-expression
    multiplicative-expression  /  cast-expression
    multiplicative-expression  %  cast-expression
    
cast-expression:
    
    unary-expression
    ( type-name )  cast-expression
    
unary-expression:
    
    postfix-expression
    ++  unary-expression
    --  unary-expression
    unary-operator  cast-expression
    sizeof  unary-expression
    sizeof  ( type-name )
    
unary-operator: one of
    
    &  *  +  -  ~  !
    
postfix-expression:
   
	primary-expression
	postfix-expression  [ expression ]
	postfix-expression  ( argument-expression-list(opt) )
	postfix-expression  .  identifier
	postfix-expression  -> identifier
	postfix-expression  ++
	postfix-expression  --
	( type-name )  { initializer-list }
	( type-name )  { initializer-list , }
    
primary-expression:
    
    identifier
    constant
    string-literal
    ( expression )
    
argument-expression-list:
    
    assignment-expression
    argument-expression-list  ,  assignment-expression
    
constant-expression:
    
    conditional-expression


####Lexical
***

token:
	
	keyword
	identifier
	constant
	string-literal
	punctuator
	
keyword: one of

	auto       enum        restrict     unsigned
	break      extern	   return       void
	case       float       short        volatile
	char       for         signed       while
	const      goto        sizeof       _Bool
	continue   if          static       _Complex
	default    inline      struct       _Imaginary
	do         int         switch
	double     long        typedef
	else       register    union
	
identifier:

	identifier-nondigit
	identifier identifier-nondigit
	identifier digit
	
identifier-nondigit:

	nondigit
	univeral-character-name
	other implemention-defined characters
	
non-digit:
	
	_  a  b  c  d  e  f  g  h  i  j  k  l  m
	   n  o  p  q  r  s  t  u  v  w  x  y  z
	   A  B  C  D  E  F  G  H  I  J  K  L  M
	   N  O  P  Q  R  S  T  U  V  W  X  Y  Z

digit: one of
    
    0  1  2  3  4  5  6  7  8  9
    
univeral-character-name:

	\u hex-quad
	\U hex-quad hex-quad
	
hex-quad:

	hexadecimal-digit hexadecimal-digit hexadecimal-digit hexadecimal-digit
	
punctuator: one of

	[    ]    (    )    {    }    .    ->
	++   --   &    *    +    -    ~    !
	/    %    <<   >>   <    >    <=   >=   ==    !=
	^    |    &&   ||
	?    :    ;    ...
	=    *=   /=    %=   +=   -=   <<=   >>=   &=   ^=   |=
	,    #    ##
	<:   :>   <%   %>   %:   %:%:

string-literal:
    
    " s-char-sequence(opt) "
    L " s-char-sequence(opt) "

s-char-sequence:
    
    s-char
    s-char-sequence  s-char
    
s-char:
    
    any member of the source character set except the double-quote ", backslash \, or new-line character
    escape-sequence
    

constant:
    
    integer-constant
    floating-constant
    enumeration-constant
    character-constant
    
integer-constant:
    
    decimal-constant  integer-suffix(opt)
    octal-conatant  integer-suffix(opt)
    hexadecimal-constant  integer-suffix(opt)
    
decimal-constant:
    
    nonzero-digit
    decimal-constant  digit
    
nonzero-digit: one of
    
    1  2  3  4  5  6  7  8  9
    
octal-constant:
    
    0
    octal-constant  octal-digit
    
octal-digit: one of
    
    0  1  2  3  4  5  6  7
    
hexadecimal-constant:
    
    hexadecimal-prefix  hexadecimal-digit
    hexadecimal-constant  hexadecimal-digit
    
hexadecimal-prefix: one of
    
    0x  0X
    
hexadecimal-digit:
    
    0  1  2  3  4  5  6  7  8  9
    a  b  c  d  e  f
    A  B  C  D  E  F
    
integer-suffix:
    
    unsigned-suffix     long-suffix(opt)
    unsigned-suffix     long-long-suffix(opt)
    long-suffix         unsigned-suffix(opt)
    long-long-suffix    unsigned-suffix(opt)

unsigned-suffix: one of
    
    u  U
    
long-suffix: one of
    
    l  L
    
long-long-suffix: one of
    
    ll  LL

enumeration-constant:
    
    identifier
    
character-constant:

	' c-char-sequence '
	L' c-char-sequence '
	
c-char-sequence:

	c-char
	c-char-sequence c-har
	
c-char:

	any member of the source character set except the single-quote ', backslash \, or new-line character
	
escape-sequence:

	simple-escape-sequence
	octal-escape-sequence
	hexadecimal-escape-sequence
	universal-character-name
	
simple-escape-sequence:

	\'    \"    \?    \\
	\a    \b    \f    \n    \r    \t    \v
	
octal-escape-sequence:

	\  octal-digit
	\  octal-digit  octal-digit
	\  octal-digit  octal-digit  octal-digit
	
hexadecimal-escape-sequence:

	\x  hexadecimal-digit
	hexadecimal-escape-sequence  hexadecimal-digit

floating-constant:

	decimal-floating-constant
	hexadecimal-floating-constant
	
decimal-floating-constant:

	fractional-constant  exponent-part(opt)  floating-suffix(opt)
	digit-sequence  exponent-part  floating-suffix(opt)
	
hexadcimal-floating-constant:

	hexadcimal-prefix  headecimal-fractional-constant binary-exponent-part  floating-suffix(opt)
	hexadcimal-prefix  headecimal-digit-sequence binary-exponent-part  floating-suffix(opt)
	
fractional-constant:

	digit-sequence(opt)  .  digit-sequence
	digit-sequence  .
	
exponent-part:

	e  sign(opt)  digit-sequence
	E  sign(opt)  digit-sequence
	
sign: one of 

	+   -
	
digit-sequence:

	digit
	digit-sequence  digit
	
hexadecimal-fractional-constant:

	hexadecimal-digit-sequence(opt)  .  hexadecimal-digit-sequence
	hexadecimal-digit-sequence  .
	
binary-exponent-part:

	p  sign(opt)  digit-sequence
	P  sign(opt)  digit-sequence
	
hexadecimal-digit-sequence:

	hexadecimal-digit
	hexadecimal-digit-sequence  hexadecimal-digit
	
floating-suffix:

	f  l  F  L

####Preprocessor
****
	
preprocessing-file:

	group(opt)
	
group:

	group-part
	group group-part
	
group-part:

	if-section
	control-line
	text-line
	# non-directive
	
if-section:

	if-group elif-groups(opt) else-group(opt) endif-line
	
if-group:

	# if constant-expression new-line group(opt)
	# ifdef identifier new-line group(opt)
	# ifndef identifier new-line group(opt)
	
elif-groups:

	elif-group
	elif-groups elif-group
	
elif-group:

	# elif constant-expression new-line group(opt)
	
else-group:

	# else new-line group(opt)
	
endif-line:

	# endif new-line
	
control-line:

	# include pp-tokens new-line
	# define identifier replacement-list new-line
	# define identifier lparen identifier-list(opt) ) replacement-list new-line
	# define identifier lparen ... ) replacement-list new-line
	# define identifier lparen identifier-list , ... ) replacement-list new-line
	# undef identifier new-line
	# line pp-tokens new-line
	# error pp-tokens(opt) new-line
	# pragma pp-tokens(opt) new-line
	# new-line
	
text-line:
	
	pp-tokens(opt) new-line
	
non-directive:

	pp-tokens new-line
	
lparen:

	a ( character not immediately preceeded by white-space
	
replacement-list:

	pp-tokens(opt)
	
pp-tokens:

	preprocessing-token
	pp-tokens preprocessing-token
	
new-line:
	
	the new-line character
	
preprocessing-token:

	header-name
	identifier
	pp-number
	character-constant
	string-literal
	punctuator
	each non-white-space character that cannot be one of the above
	
header-name:
	
	< h-char-sequence >
	" q-char-sequence "
	
h-char-sequence:

	h-char
	h-char-sequence h-char
	
h-char:

	any member of the source character set except the new-line character and >
	
q-char-sequence:

	q-char
	q-char-sequence q-char
	
q-char:

	any member of the source character set except the new-line character and "
	
pp-number:

	digit
	. digit
	pp-number digit
	pp-number identifer-nondigit
	pp-number e sign
	pp-number E sign
	pp-number p sign
	pp-number P sign
	pp-number .
	

