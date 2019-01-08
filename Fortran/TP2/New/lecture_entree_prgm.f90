PROGRAM lecture_entree_prgm
IMPLICIT none

! faire un allocatable pour une matrice m,n qui stockera les variables du fichier
!initialisation matrice entree
INTEGER                            :: i,m,n !m lignes, n colonnes
REAL, DIMENSION(:,:),Allocatable   :: ent


OPEN(3,file="entree.txt")

WRITE(*,*) "nombre de lignes"
READ(*,*),m
WRITE(*,*) "nombre de colonnes"
READ(*,*),n
ALLOCATE (ent(m,n))

do i=1,m
    READ(3,*),ent(i,:)
end do


CLOSE(3)

DEALLOCATE(ent)

END PROGRAM
