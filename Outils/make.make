#Compilateur
CC=gfortran
#Options de compilation fortran
FFLAGS=-Wall
LDFLAGS=
#nom de l'executable
EXENAME=.exe
#chemin relatif des dossiers
SRCDIR=
OBJDIR=
#tous les *.f90 sont consideres comme des sources
SRC=$(SRCDIR)$(wildcard *.f90)
#on substitue les extensions .f90 en .o pour les objets
OBJ=$(SRC:.f90=.o)
#ou OBJ=$(subst .f90,.o,$(SRC))

all: $(EXENAME)

$(EXENAME): $(OBJ)
	$(CC) -o $@ $^ $(LDFLAGS)

#.SUFFIXES: .f90
#.f90.o
#	$(CC) -c $(FFLAGS) $<
%.o: %.f90
	$(CC) -o $@ -c $< $(FFLAGS)

clean:
	rm -f $(OBJDIR)
	rm -f *.exe