PROGRAM genentree
IMPLICIT none

INTEGER i,j,m,n
REAL, DIMENSION(:,:),Allocatable :: matentreealeat

!demande dimension n lignes, m colonnes
WRITE(*,*)"Rentrer m lignes"
READ(*,*)m
WRITE(*,*)"Rentrer n colonnes"
READ(*,*)n

ALLOCATE (matentreealeat(m,n))

CALL random_number(matentreealeat(:,:))

OPEN(1,file="entree.txt")

Do i = 1, m
  WRITE(1,*) matentreealeat(i,:)
End do

DEALLOCATE(matentreealeat)
close(1)


END PROGRAM genentree
