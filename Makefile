LIBS=-lraylib -lGL -lm -lpthread -ldl -lrt -lX11 -llapack -lglfw -lblas

all: libray
	gfortran -O2 -ffree-line-length-512 -g -L/home/slamko/.local/lapack -L/home/slamko/src/fortran-raylib -L/home/slamko/proj/gfortran/engine/ -Wl,-R/home/slamko/proj/gfortran/engine/ -I/home/slamko/src/fortran-raylib engine.f90 /home/slamko/.local/lapack/liblapack.a -lfortran-raylib $(LIBS) -g -o engine


libray:
	make -C ray
