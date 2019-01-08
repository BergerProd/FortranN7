PROGRAM tp3
IMPLICIT none
INTEGER         ::m,n,choix,npts
REAL(KIND=8),DIMENSION(:,:),ALLOCATABLE :: ent
CHARACTER(len=100):: nom

PRINT*,' Options possibles :'
PRINT*,'     < 1 > : question 1'
PRINT*,'     < 2 > : question 2'
PRINT*,'     < 3 > : question 3'
PRINT*,'     < 4 > : question 4'
PRINT*, 'Entrer votre choix :'
READ(*,*)choix

WRITE(*,*)"nombre de ligne, nombre de colonnes, nom du fichier"
READ(*,*)m,n,nom

SELECT CASE (choix)
CASE (1)
  CALL genere_entree_aleat_sub(m,n,nom)
CASE(2)
  CALL genere_sortie_aleat_sub(npts,nom)
CASE(3)
  CALL lecture_entree_sub(m,n,nom,ent)
CASE(4)
  WRITE(*,*)"pas encore réalisé"
CASE DEFAULT
  WRITE(*,*)"le nombre doit être compris entre 1 et 4"
END SELECT

END PROGRAM tp3
