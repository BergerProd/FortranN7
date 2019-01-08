!*******************************************
SUBROUTINE genere_fichier_aleatoire(m,n,nom)
!*******************************************

IMPLICIT NONE
INTEGER,INTENT(IN)          :: n,m
CHARACTER(LEN=*)            :: nom
REAL(kIND=8),DIMENSION(n)   :: x
INTEGER                     :: i,j

OPEN(1,FORM='FORMATTED',FILE=nom)
DO i=1,m
    DO j=1,n
        x(j)=RAN()
    END DO
    WRITE(1,*)x
END DO

CLOSE(1)

END SUBROUTINE genere_fichier_aleatoire



