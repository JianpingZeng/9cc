# Makefile

CFLAGS = -Wall -std=c99 -I..
TARGET_SPEC = x86_64-linux.7
TARGET_OBJ = x86_64-linux.o
TARGET_SRC = x86_64-linux.c
RM = @rm -f

$(TARGET_OBJ): $(TARGET_SRC)
	$(CC) $(CFLAGS) -c $(TARGET_SRC) -o $@

$(TARGET_SRC): $(TARGET_SPEC)
	$(BURG) $(TARGET_SPEC) -o $@

clean::
	$(RM) *.o $(TARGET_SRC)
