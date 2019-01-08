FC= gfortran
FFLAGS= -fdefault-real-8 -Wsurprising  -ffree-line-length-none -O3 -Wall 

EXENAME=../RUN/tp2_modules.bin			# nom du programme, dans le dossier au-dessus

# Déclaration des objets
OBJS =  \
 module_tp2.o\
 tp2.o
ALL: $(EXENAME)

.SUFFIXES: .o .f90

.f90.o: module_tp2.o			# ajouter les modules supplémentaires sur cette ligne
	$(FC)  -c $(FFLAGS)  $<

$(EXENAME):   $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) 

# lignes à écrire uniquement pour chacun des modules
module_tp2.o: 					
	$(FC) $(FFLAGS) -c module_tp2.f90

# pour compiler à nouveau
clean:
	/bin/rm *.o *.mod *.f90~  Makefile~  $(EXENAME)

