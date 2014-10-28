FC = gfortran
FFLAGS := -O3

all: euler_mod.o

%.o: %.f08
	$(FC) $(FFLAGS) -c $<

clean:
	$(RM) *.o *.mod
