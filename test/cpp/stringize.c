#define TEST(a, b)  printf( #a "<" #b "=%d\n", (a) < (b) )
TEST('\n', 10)

#undef TEST
#define TEST(a)  # a
TEST(func(a,b))
TEST (func(a,b))
TEST(func(a, b))
TEST (func(a, b))
TEST(  func (  a  ,  b  )  )
TEST (  func (  a  ,  b  )  )

#undef TEST
#define TEST(a, b)   a # b
TEST (abc, cde)

#undef TEST
#define TEST(a, b)   a# b
TEST (abc, cde)

#undef TEST
#define TEST(a, b) a #b
TEST (abc, cde)

#undef TEST
#define TEST(...)  # __VA_ARGS__
TEST (a)
TEST (a,b,c)
TEST (a, b, c)
TEST (  a  ,  b  ,  c  )
TEST (  func ( a , b ) , h )
