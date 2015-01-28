BUILD=$(BUILDDIR)/objects/lib
CC=gcc
CFLAGS=-c -Wall -std=c99
AR=ar
ARFLAGS=crv
SOURCES=$(wildcard *.c)
OBJECTS=$(addprefix $(BUILD)/, $(SOURCES:.c=.o))
ARCHIVE=$(BUILDDIR)/libcc.a

all: $(SOURCES) $(ARCHIVE)

$(ARCHIVE): $(OBJECTS)
	$(AR) $(ARFLAGS) $@ $(OBJECTS)

$(BUILD)/%.o: %.c
	@mkdir -p $(BUILD)
	$(CC) $(CFLAGS) $< -o $@

.PHONY: clean 

clean:
	@rm -f $(OBJECTS) $(ARCHIVE)
