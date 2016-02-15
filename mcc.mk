# makefile for Windows NT

CC=cl /nologo /MT
LD=link /nologo
CFLAGS=/D CONFIG_WINNT /D "BUILD_DIR=\"$(MAKEDIR)\""
LDFLAGS=/MACHINE:X64 rpcrt4.lib shlwapi.lib
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
	
$(SYS_OBJ): $(SYS)winnt.c $(SYS_INC)
	$(CC) $(CFLAGS) /c /Fo$@ $(SYS)winnt.c
	
$(UTILS)wrapper$O: $(UTILS)wrapper.c $(UTILS_INC)
	$(CC) $(CFLAGS) /c /Fo$@ $(UTILS)wrapper.c
	
$(UTILS)strbuf$O: $(UTILS)strbuf.c	$(UTILS_INC)
	$(CC) $(CFLAGS) /c /Fo$@ $(UTILS)strbuf.c
	
$(UTILS)vector$O: $(UTILS)vector.c	$(UTILS_INC)
	$(CC) $(CFLAGS) /c /Fo$@ $(UTILS)vector.c

$(UTILS)map$O: $(UTILS)map.c  $(UTILS_INC)
	$(CC) $(CFLAGS) /c /Fo$@ $(UTILS)map.c
	
$(UTILS)string$O: $(UTILS)string.c  $(UTILS_INC)
	$(CC) $(CFLAGS) /c /Fo$@ $(UTILS)string.c
	
$(UTILS)hideset$O: $(UTILS)hideset.c  $(UTILS_INC)
	$(CC) $(CFLAGS) /c /Fo$@ $(UTILS)hideset.c
	
$(UTILS)dict$O: $(UTILS)dict.c  $(UTILS_INC)
	$(CC) $(CFLAGS) /c /Fo$@ $(UTILS)dict.c
	
alloc$O: alloc.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ alloc.c

ast$O: ast.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ ast.c
	
cc$O: cc.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ cc.c
	
cpp$O: cpp.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ cpp.c
	
print$O: print.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ print.c
	
decl$O: decl.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ decl.c
	
error$O: error.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ error.c
	
eval$O: eval.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ eval.c
	
expr$O: expr.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ expr.c
	
gen$O: gen.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ gen.c
	
lex$O: lex.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ lex.c
	
stmt$O: stmt.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ stmt.c
	
sym$O: sym.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ sym.c
	
type$O: type.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ type.c
	
input$O: input.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ input.c
	
initializer$O: initializer.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ initializer.c
	
ir$O: ir.c $(CC1_INC)
	$(CC) $(CFLAGS) /c /Fo$@ ir.c
	
$(MCC): $(MCC_OBJ) $(CC1_OBJ) $(SYS_OBJ)
	$(LD) $(LDFLAGS) /out:$@ $(MCC_OBJ) $(SYS_OBJ) $(CC1_OBJ)

clean::
	-del /q *.obj
	-del /q $(MCC)
	-del /q $(UTILS)*.obj
	-del /q $(SYS)*.obj