CC=gcc
CFLAGS=-c -Wall
LDFLAGS=
SOURCES=$(wildcard *.c)
OBJECTS=$(SOURCES:.cpp=.o)
EXECUTABLE=cc

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS) 
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@

.PHONY: clean

clean:
	rm -f *.o $(EXECUTABLE)
