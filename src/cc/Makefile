CC=gcc
LIBS=libcc.a
LIB=lib
CFLAGS=-c -Wall -I$(LIB) -std=c99
LDFLAGS=-L$(LIB) -lcc
SOURCES=$(wildcard *.c)
OBJECTS=$(SOURCES:.c=.o)
EXECUTABLE=cc

all: $(SOURCES) $(EXECUTABLE) $(LIBS)

$(EXECUTABLE): $(OBJECTS) $(LIBS)
	$(CC) $(OBJECTS) $(LDFLAGS) -o $@

$(LIBS):
	@cd $(LIB) && make

.c.o:
	$(CC) $(CFLAGS) $< -o $@

.PHONY: clean

clean:
	@rm -f *.o $(EXECUTABLE)
	@cd $(LIB) && make clean
