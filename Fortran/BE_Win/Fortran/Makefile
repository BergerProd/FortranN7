FC= gfortran
#FFLAGS= -fdefault-real-8 -Wsurprising  -ffree-line-length-none -O3 -Wall
FFLAGS=  -Wsurprising  -ffree-line-length-none -O3 -Wall -g

EXENAME=../RUN/main.bin			# nom du programme, dans le dossier au-dessus

# Déclaration des objets
OBJS =  \
 mod_balistique.o\
 main.o \
 subroutines.o

ALL: $(EXENAME)

.SUFFIXES: .o .f90

.f90.o: mod_balistique.o			# ajouter les modules supplémentaires sur cette ligne
	$(FC)  -c $(FFLAGS)  $<

$(EXENAME):   $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS)

# lignes à écrire uniquement pour chacun des modules
mod_balistique.o:
	$(FC) $(FFLAGS) -c mod_balistique.f90

# pour compiler à nouveau
clean:
	/bin/rm *.o *.mod *.f90~  Makefile~  $(EXENAME)
