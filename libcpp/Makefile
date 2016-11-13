
CFLAGS = -Wall -std=c99 -I..
LIBCPP = libcpp.a
LIBCPP_OBJ =
LIBCPP_INC =
RM = @rm -f
AR = ar
ARFLAGS = cru
CONFIG_H = ../config.h

LIBCPP_OBJ += lex.o
LIBCPP_OBJ += cpp.o
LIBCPP_OBJ += hideset.o
LIBCPP_OBJ += imap.o
LIBCPP_OBJ += input.o
LIBCPP_OBJ += error.o
LIBCPP_OBJ += expr.o

LIBCPP_INC += lex.h
LIBCPP_INC += token.def
LIBCPP_INC += internal.h

$(LIBCPP): $(LIBCPP_OBJ) $(LIBCPP_INC) $(CONFIG_H)
	$(AR) $(ARFLAGS) $@ $(LIBCPP_OBJ)

clean::
	$(RM) *.o $(LIBCPP)
