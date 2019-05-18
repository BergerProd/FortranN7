FC= gfortran
#FFLAGS= -fdefault-real-8 -Wsurprising  -ffree-line-length-none -O3 -Wall 
FFLAGS=  -Wsurprising  -ffree-line-length-none -O3 -Wall 

EXENAME=../RUN/main.bin			# nom du programme, dans le dossier au-dessus

# Déclaration des objets
OBJS =  \
 module_reacteur_chimique.o\
 main.o \
 subroutine.o

ALL: $(EXENAME)

.SUFFIXES: .o .f90

.f90.o: module_reacteur_chimique.o			# ajouter les modules supplémentaires sur cette ligne
	$(FC)  -c $(FFLAGS)  $<

$(EXENAME):   $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) 

# lignes à écrire uniquement pour chacun des modules
module.o: 					
	$(FC) $(FFLAGS) -c module_reacteur_chimique.f90

# pour compiler à nouveau
clean:
	/bin/rm *.o *.mod *.f90~  Makefile~  $(EXENAME)

