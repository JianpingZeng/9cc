ifndef BUILDDIR
BUILDDIR=../build
endif

BUILD=$(BUILDDIR)/objects/cc
CC=cc
CFLAGS=-c -Wall -std=c99
LDFLAGS=
SOURCES=$(wildcard *.c)
OBJECTS=$(addprefix $(BUILD)/, $(SOURCES:.c=.o))
EXECUTABLE=$(BUILDDIR)/cc
INCLUDES=$(wildcard *.h)

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(OBJECTS) $(LDFLAGS) -o $@

$(BUILD)/%.o: %.c $(INCLUDES)
	@mkdir -p $(BUILD)
	$(CC) $(CFLAGS) $< -o $@

.PHONY: clean

clean:
	@rm -f $(OBJECTS) $(EXECUTABLE)
