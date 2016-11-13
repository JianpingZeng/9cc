
CFLAGS = -Wall -std=c99 -I..
LIBUTILS = libutils.a
LIBUTILS_OBJ =
LIBUTILS_INC =
RM = @rm -f
AR = ar
ARFLAGS = cru
CONFIG_H = ../config.h

LIBUTILS_OBJ += alloc.o
LIBUTILS_OBJ += wrapper.o
LIBUTILS_OBJ += strbuf.o
LIBUTILS_OBJ += vector.o
LIBUTILS_OBJ += string.o
LIBUTILS_OBJ += list.o
LIBUTILS_OBJ += sys.o

LIBUTILS_INC += utils.h
LIBUTILS_INC += strbuf.h
LIBUTILS_INC += vector.h
LIBUTILS_INC += list.h
LIBUTILS_INC += sys.h
LIBUTILS_INC += color.h

$(LIBUTILS): $(LIBUTILS_INC) $(LIBUTILS_OBJ) $(CONFIG_H)
	$(AR) $(ARFLAGS) $@ $(LIBUTILS_OBJ)

clean::
	$(RM) *.o $(LIBUTILS)
