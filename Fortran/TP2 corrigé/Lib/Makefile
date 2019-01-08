FC= gfortran
FFLAGS= -fdefault-real-8 -Wsurprising  -ffree-line-length-none -O3 -Wall 
LIB_PERSO=./Librairie/lib_tp2.a


EXENAME=../RUN/tp2_lib.bin			# nom du programme, dans le dossier au-dessus


# Déclaration des objets
OBJS =  \
tp2.o
ALL: $(EXENAME)

.SUFFIXES: .o .f90

.f90.o: 
	$(FC)  -c $(FFLAGS)  $<

$(EXENAME):   $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LIB_PERSO)


# pour compiler à nouveau
clean:
	/bin/rm *.o *.mod *.f90~  Makefile~  $(EXENAME)

