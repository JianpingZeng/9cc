CC=gcc
CFLAGS=-c -Wall -std=c99
LDFLAGS=
INCLUDES=$(wildcard *.h)

ifeq ($(platform), mac)
SOURCES=cpp.c mac.c
endif

ifeq ($(platform), linux)
SOURCES=cpp.c linux.c
endif

ifeq ($(platform), windows)
SOURCES=cpp.c windows.c
endif

OBJECTS=$(SOURCES:.c=.o)
EXECUTABLE=cpp

ifdef SOURCES
all: $(SOURCES) $(EXECUTABLE)
else
all:
	@echo "Usage: make platform=<mac, linux or windows>"
	@exit 1
endif

$(EXECUTABLE): $(OBJECTS) 
	$(CC) $(OBJECTS) $(LDFLAGS) -o $@

%.o: %.c $(INCLUDES)
	$(CC) $(CFLAGS) $< -o $@

.PHONY: clean

clean:
	@rm -f *.o $(EXECUTABLE)
