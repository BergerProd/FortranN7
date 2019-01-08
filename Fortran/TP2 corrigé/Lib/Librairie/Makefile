FC= gfortran
FFLAGS= -fdefault-real-8 -Wsurprising  -ffree-line-length-none -O3 -Wall 

lib=./lib_tp2

# Déclaration des objets
OBJS_LIB =  \
menu.o \
genere_fichier_aleatoire.o\
genere_fichier_fonction.o\
genere_fichier_binaire.o\
lire_fichier.o

lib_new : $(OBJS_LIB)
	ar -r lib_tp2.a $?

.SUFFIXES: .o .f90

.f90.o:
	$(FC)  -c $(FFLAGS)  $<

# pour compiler à nouveau
clean:
	/bin/rm *.o *.mod *.f90~  Makefile~  *.a

