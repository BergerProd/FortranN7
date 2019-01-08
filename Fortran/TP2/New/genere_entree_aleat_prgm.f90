PROGRAM genere_entree_aleat
!Genere un fichier de nombre aléatoire entre 0 et 1 a n lignes et m colonnes

IMPLICIT none
!déclaration des variables
REAL,DIMENSION(:),Allocatable :: u !vecteur à allocation dynamique
INTEGER                       :: m,n,i ! m lignes, n colonnes
!ouverture du fichier
OPEN(1,file="entree.txt")
!demande dimension n lignes, m colonnes
WRITE(*,*)"Rentrer m lignes"
READ(*,*)m
WRITE(*,*)"Rentrer n colonnes"
READ(*,*)n
!allocation dynamique du vecteur avec n colonnes
ALLOCATE(u(n))

do i=1,m !pour chaque ligne
  CALL random_number(u(:))!remplissage du vecteur tout entier avec un nombre aléatoire ******** formatter
  WRITE(1,*),u(:)!écriture dans le fichier du vecteur tout entier
END do


close(1)!fermeture du fichier
DEALLOCATE (u)
END PROGRAM genere_entree_aleat
