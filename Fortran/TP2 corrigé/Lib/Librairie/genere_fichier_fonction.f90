!****************************************
SUBROUTINE  genere_fichier_fonction(n,nom)
!****************************************

IMPLICIT NONE
INTEGER,INTENT(IN)          :: n
CHARACTER(LEN=*)            :: nom
REAL(kIND=8)                :: x,dx
INTEGER                     :: i

dx=16.d0*DATAN(1.d0)/DFLOAT(n-1)
x=0.d0
OPEN(1,FORM='FORMATTED',FILE=nom)
DO i=1,n
    WRITE(1,'(4(e12.5,3x))')x,COS(x),SIN(x),EXP(x)
    x=x+dx
END DO
CLOSE(1)

END SUBROUTINE genere_fichier_fonction

