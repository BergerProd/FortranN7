FC= gfortran
#FFLAGS= -fdefault-real-8 -Wsurprising  -ffree-line-length-none -O3 -Wall 
FFLAGS = -fcheck=all -O0 

EXENAME= Volumes_finis.bin			# nom du programme, dans le dossier au-dessus

# Déclaration des objets
OBJS =  \
 module_VF.o\
 Volumes_finis.o\
 lecture_fichier.o\
 subroutines.o\
 VTSWriter.o

ALL: $(EXENAME)

.SUFFIXES: .o .f90

.f90.o: module_VF.o			# ajouter les modules supplémentaires sur cette ligne
	$(FC)  -c $(FFLAGS)  $<

$(EXENAME):   $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) 

# lignes à écrire uniquement pour chacun des modules
module_VF.o: 			
	$(FC) $(FFLAGS) -c module_VF.f90

# pour compiler à nouveau
clean:
	/bin/rm -f *.o *.mod *.f90~  Makefile~  $(EXENAME)

