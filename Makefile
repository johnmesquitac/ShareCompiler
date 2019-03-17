all: tokenizer parser build

tokenizer:
	flex cemenos.l

parser:
	bison -d cemenos.y

build:
	gcc -c *.c -fno-builtin-exp -Wno-implicit-function-declaration
	gcc *.o -lfl -o cemenos -fno-builtin-exp

clean:
	rm -f cemenos
	rm -f lex.yy.c
	rm -f *.o
	rm -f cemenos.tab.*
