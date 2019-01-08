!*******************************************
SUBROUTINE genere_fichier_binaire(m,n,nom)
!*******************************************

IMPLICIT NONE
INTEGER,INTENT(IN)          :: n,m
CHARACTER(LEN=*)            :: nom
REAL(kIND=8),DIMENSION(m,n) :: x
REAL(kIND=8),DIMENSION(m,2) :: y
INTEGER                     :: i


OPEN(1,FORM='FORMATTED',FILE=nom)
DO i=1,m
    READ(1,*)x(i,:)
END DO
CLOSE(1)

OPEN(1,FORM='UNFORMATTED',FILE='sortie.bin')
DO i=1,m
    WRITE(1) x(i,1),SUM(x(i,2:n))
END DO
CLOSE(1)

! autre possibilité, voir les différences
y(:,1)=x(:,1)
DO i=1,m
    y(i,2)=SUM(x(i,2:n))
END DO
OPEN(1,FORM='UNFORMATTED',FILE='sortie1.bin')
WRITE(1) y
CLOSE(1)

END SUBROUTINE genere_fichier_binaire

