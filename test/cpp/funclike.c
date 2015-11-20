#define F() f
a + F() + b
#undef F
#define F()
    a + F() + b a + F() + b
