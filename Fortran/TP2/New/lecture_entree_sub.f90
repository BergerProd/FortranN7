SUBROUTINE lecture_entree_sub(m,n,nom,ent)
  !lecture d'un fichier que l'on stocke dans une matrice
  !Input : m lignes, n colonnes, nom du fichier
  !Output : ent matrice rempli du fichier
IMPLICIT none



INTEGER                                       :: i
INTEGER,INTENT(IN)                            :: m,n !m lignes, n colonnes
CHARACTER(len=*),INTENT(IN)                   :: nom
REAL,DIMENSION(:,:),Allocatable,INTENT(OUT)   :: ent


OPEN(3,file=nom)
ALLOCATE (ent(m,n))

DO i=1,m
    READ(3,*),ent(i,:)
END DO


CLOSE(3)
DEALLOCATE(ent)

END SUBROUTINE
