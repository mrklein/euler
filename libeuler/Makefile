FC = gfortran
FFLAGS := -g

all: euler_mod.o

%.o: %.f08
	$(FC) $(FFLAGS) -c $<

clean:
	$(RM) *.o *.mod
