LIBS=-lraylib -lGL -lm -lpthread -ldl -lrt -lX11 -llapack -lglfw -lblas
SRC=$(wildcard src/*.f90)

all: libray
	gfortran -Wall -Wno-unused-variable -ffree-line-length-512 -g -L/home/slamko/.local/lapack -L/home/slamko/src/fortran-raylib -L/home/slamko/proj/gfortran/engine/raylib/lib -Wl,-R/home/slamko/proj/gfortran/engine/raylib/lib -I/home/slamko/src/fortran-raylib $(SRC) /home/slamko/.local/lapack/liblapack.a -lfortran-raylib $(LIBS) -g -o engine


libray:
	make -C ray
