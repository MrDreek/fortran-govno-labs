FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -Wno-unused-function -static-libgfortran -flto
FOPT=-O3 -ftree-vectorize -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/io.f90 -J obj/ -o obj/io.o
	$(FC) $(FFLAGS) $(FOPT) -c src/process.f90 -J obj/ -o obj/process.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app ./obj/environment.o ./obj/io.o ./obj/process.o ./obj/main.o
	make run

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
