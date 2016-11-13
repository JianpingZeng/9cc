# Makefile

CFLAGS = -Wall -std=c99
7BURG = 7burg
RM = @rm -f

$(7BURG): 7burg.c 7burg.h grammar.c
	$(CC) $(CFLAGS) grammar.c 7burg.c -o $@

clean::
	$(RM) $(7BURG) *.o
