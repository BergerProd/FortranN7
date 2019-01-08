FC= gfortran
FFLAGS= -fdefault-real-8 -Wsurprising  -ffree-line-length-none -O3 -Wall 

EXENAME=../RUN/tp2_simple.bin			# nom du programme, dans le dossier au-dessus

# Déclaration des objets
OBJS =  tp2.o

ALL: $(EXENAME)

.SUFFIXES: .o .f90

.f90.o: 
	$(FC)  -c $(FFLAGS)  $<

$(EXENAME):   $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) 

# pour compiler à nouveau
clean:
	/bin/rm *.o *.mod *.f90~  Makefile~  $(EXENAME)

