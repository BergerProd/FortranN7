PROGRAM tp2 
! Novembre 2013
! C. Airiau
!************************************
! DECLARATION DES VARIABLES
!************************************

IMPLICIT NONE
INTEGER             :: option
INTEGER,PARAMETER   :: n=5, m=10, np=201
REAL(KIND=8),DIMENSION(n,m) :: mat

CALL menu(option)       

SELECT CASE (option)
    case(1)
        CALL genere_fichier_aleatoire(m,n,'entree.txt')

    case(2)
        CALL genere_fichier_fonction(np,'data.dat')

    case(3)
        CALL lire_fichier(m,n,'entree.txt',mat)
        PRINT*, 'Matrice lu : '
        PRINT*,mat
        PRINT*
        PRINT*
        ! voir la difference avec 
        PRINT'(5e15.5)',mat

    case(4)

        CALL genere_fichier_binaire(m,n,'entree.txt')

END SELECT



END PROGRAM tp2

