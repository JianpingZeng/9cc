# makefile for Windows NT
CC=cl /nologo
LD=cl /nologo
CFLAGS=/Wall /DBUILD_DIR=$(MAKEDIR)
LDFLAGS=/MT
UTILS=utils\
SYS=sys\
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
SYS_OBJ:=$(SYS)winnt$O

MCC_OBJ=mcc$O

all: $(MCC)

$(MCC): $(MCC_OBJ) $(CC1_OBJ) $(SYS_OBJ)
	$(LD) $(LDFLAGS) /Fe$@ $(MCC_OBJ) $(SYS_OBJ) $(CC1_OBJ)

$(CC1_OBJ): $(CC1_INC)

$(SYS_OBJ): $(SYS_INC)

clean::
	-del /q *.obj
	-del /q $(MCC)
