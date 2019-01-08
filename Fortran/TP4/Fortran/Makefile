FC= gfortran
#FFLAGS= -fdefault-real-8 -Wsurprising  -ffree-line-length-none -O3 -Wall
FFLAGS=  -Wsurprising  -ffree-line-length-none -O3 -Wall -g

EXENAME=../RUN/tp4.bin			# nom du programme, dans le dossier au-dessus

# Déclaration des objets
OBJS =  \
 module_tp4.o\
 tp4.o \
 subroutines.o

ALL: $(EXENAME)

.SUFFIXES: .o .f90

.f90.o: module_tp4.o			# ajouter les modules supplémentaires sur cette ligne
	$(FC)  -c $(FFLAGS)  $<

$(EXENAME):   $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS)

# lignes à écrire uniquement pour chacun des modules
module_tp4.o:
	$(FC) $(FFLAGS) -c module_tp4.f90

# pour compiler à nouveau
clean:
	/bin/rm *.o *.mod *.f90~  Makefile~  $(EXENAME)
