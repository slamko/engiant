shared:
	gcc -O2 -c -g -fpic engine.c
	gcc -shared -o libraylib.so engine.o
	cp libraylib.so ..

all:
	gcc engine.c -lm -lglfw -lGL -ldl -o engine

lib:
	gcc -c engine.c

