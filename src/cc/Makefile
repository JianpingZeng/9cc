CC=gcc
LIBDIR=../lib
LIBCC=../lib/libcc.a
CFLAGS=-c -Wall -std=c99 -I$(LIBDIR)
LDFLAGS=-L$(LIBDIR) -lcc
SOURCES=$(wildcard *.c)
OBJECTS=$(SOURCES:.c=.o)
EXECUTABLE=cc
INCLUDES=$(wildcard *.h)

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS) $(LIBCC)
	$(CC) $(OBJECTS) $(LDFLAGS) -o $@

%.o: %.c $(INCLUDES)
	$(CC) $(CFLAGS) $< -o $@

.PHONY: clean

clean:
	@rm -f *.o $(EXECUTABLE)
