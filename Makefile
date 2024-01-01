LIBS=-lraylib -lGL -lm -lpthread -ldl -lrt -lX11 -llapack -lglfw -lblas

all: libray
	gfortran -Wall -ffree-line-length-512 -g -L/home/slamko/.local/lapack -L/home/slamko/src/fortran-raylib -L/home/slamko/proj/gfortran/engine/raylib/lib -Wl,-R/home/slamko/proj/gfortran/engine/raylib/lib -I/home/slamko/src/fortran-raylib src/types.f90 src/math.f90 src/physics.f90 src/verlet.f90 /home/slamko/.local/lapack/liblapack.a -lfortran-raylib $(LIBS) -g -o engine


libray:
	make -C ray
