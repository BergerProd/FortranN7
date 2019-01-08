!*******************************************
SUBROUTINE lire_fichier(m,n,nom,x)
!*******************************************

IMPLICIT NONE
INTEGER,INTENT(IN)          :: n,m
CHARACTER(LEN=*)            :: nom
REAL(kIND=8),DIMENSION(m,n),INTENT(OUT) :: x
INTEGER                     :: i


OPEN(1,FORM='FORMATTED',FILE=nom)
DO i=1,m
    READ(1,*)x(i,:)
END DO
CLOSE(1)


END SUBROUTINE lire_fichier

