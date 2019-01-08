PROGRAM readentree
IMPLICIT none

!initialisation matrice entree
REAL, DIMENSION(:,:),Allocatable :: matentree
REAL :: lignes,colonnes

OPEN(3,file="entree.txt")
READ(3,*)

CLOSE(3)

END PROGRAM
