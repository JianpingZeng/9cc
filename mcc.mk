# makefile for Windows NT

CC=cl /nologo
LD=cl /nologo
CFLAGS=/Wall /DBUILD_DIR=$(MAKEDIR)
LDFLAGS=/MT
UTILS=utils^\
SYS=sys^\
O=.obj
E=.exe
MCC=mcc$E

UTILS_OBJ=$(UTILS)wrapper$O \
	$(UTILS)strbuf$O \
	$(UTILS)vector$O \
	$(UTILS)map$O \
	$(UTILS)string$O \
	$(UTILS)hideset$O \
	$(UTILS)dict$O

UTILS_INC= $(UTILS)strbuf.h \
	$(UTILS)vector.h \
	$(UTILS)map.h \
	$(UTILS)hideset.h \
	$(UTILS)dict.h \
	$(UTILS)utils.h

CC1_OBJ=alloc$O \
	ast$O \
	cc$O \
	cpp$O \
	print$O \
	decl$O \
	error$O \
	eval$O \
	expr$O \
	gen$O \
	lex$O \
	stmt$O \
	sym$O \
	type$O \
	input$O \
	initializer$O \
	ir$O \
	$(UTILS_OBJ)

CC1_INC=cc.h \
	ast.h \
	mcc.h \
	node.def \
	token.def \
	gen.h \
	rop.def \
	lex.h \
	$(UTILS_INC)

SYS_INC=$(SYS)sys.h
SYS_OBJ=$(SYS)winnt$O
MCC_OBJ=mcc$O

all: $(MCC)

$(MCC_OBJ): mcc.c
	$(CC) $(CFLAGS) /c /Fo$@ mcc.c
	
$(UTILS)wrapper$O: $(UTILS)wrapper.c $(UTILS_INC)
	$(CC) $(CFLAGS) /c /Fo$@ $(UTILS)wrapper.c
	
$(UTILS)strbuf$O: $(UTILS)strbuf.c	$(UTILS_INC)
	$(CC) $(CFLAGS) /c /Fo$@ $(UTILS)strbuf.c
	
$(UTILS)vector$O: $(UTILS)vector.c	$(UTILS_INC)
	$(CC) $(CFLAGS) /c /Fo$@ $(UTILS)vector.c

$(UTILS)map$O: $(UTILS)map.c  $(UTILS_INC)
	$(CC) $(CFLAGS) /c /Fo$@ $(UTILS)map.c
	
alloc$O: alloc.c
	$(CC) $(CFLAGS) /c /Fo$@ alloc.c

ast$O: ast.c
	$(CC) $(CFLAGS) /c /Fo$@ ast.c
	
$(MCC): $(MCC_OBJ) $(CC1_OBJ) $(SYS_OBJ)
	$(LD) $(LDFLAGS) /Fe$@ $(MCC_OBJ) $(SYS_OBJ) $(CC1_OBJ)

clean::
	-del /q *.obj
	-del /q $(MCC)
