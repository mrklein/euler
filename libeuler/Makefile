FC = gfortran

all: euler_mod.o

%.o: %.f08
	$(FC) -c $<

clean:
	$(RM) *.o *.mod
