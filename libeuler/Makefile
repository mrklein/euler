FC = gfortran

all: euler_mod.o

%.o: %.f08
	$(FC) -O3 -c $<

clean:
	$(RM) *.o *.mod
