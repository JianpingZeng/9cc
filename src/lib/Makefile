CC=gcc
CFLAGS=-c -Wall -std=c99
AR=ar
ARFLAGS=crv
SOURCES=$(wildcard *.c)
OBJECTS=$(SOURCES:.c=.o)
ARCHIVE=libcc.a

all: $(SOURCES) $(ARCHIVE)

$(ARCHIVE): $(OBJECTS)
	$(AR) $(ARFLAGS) $@ $(OBJECTS)

.c.o:
	$(CC) $(CFLAGS) $< -o $@

.PHONY: clean

clean:
	@rm -f *.o $(ARCHIVE)
