!***************************************
SUBROUTINE genere_entree_aleat(m,n,nom)
!***************************************
!Genere un fichier de nombre aléatoire entre 0 et 1 a n lignes et m colonnes

IMPLICIT none
!déclaration des variables
REAL(KIND=8),DIMENSION(:),Allocatable :: u !vecteur à allocation dynamique
INTEGER,INTENT(IN)            :: m,n ! m lignes, n colonnes
INTEGER                       :: i
CHARACTER(len=*),INTENT(IN)   :: nom !nom du fichier en entrée

!ouverture du fichier
OPEN(1,file=nom)
!allocation dynamique du vecteur avec n colonnes
ALLOCATE(u(n))

DO i=1,m !pour chaque ligne
  CALL random_number(u(:))!remplissage du vecteur tout entier avec un nombre aléatoire
  WRITE(1,*),u(:)!écriture dans le fichier du vecteur tout entier
END do

CLOSE(1)
DEALLOCATE (u)

END SUBROUTINE genere_entree_aleat
