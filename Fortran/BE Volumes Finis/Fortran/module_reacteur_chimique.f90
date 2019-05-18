MODULE module_reacteur_chimique

IMPLICIT NONE

INTEGER                                 :: nptx, npty
REAL(KIND=8)                            :: dt, tfinal, l, a, alpha_a, alpha_b,pi
REAL(KIND=8),ALLOCATABLE,DIMENSION(:)   :: x, y, ux, uy

CONTAINS

!****************************
FUNCTION carac(N_in,i_enter)
!****************************
!
!  conversion d'un nombre entier en une chaine de caractère
!  exemple : 123 --> '123'
!  N_in : nombre de chiffres
!  i_enter : nombre entier à convertir

IMPLICIT NONE
INTEGER, INTENT(IN) :: i_enter,N_in
CHARACTER(LEN=N_in) :: carac
INTEGER             :: i,k,n

IF (N_in.le.1) THEN
        WRITE(*,*) 'Problème avec la fonction carac'
        STOP
ENDIF

n=N_in-1
i=i_enter
DO k=n,0,-1
        carac(n-k+1:n-k+1)=CHAR(48+INT(i/10**k))
        i=MOD(i,10**k)
END DO
END FUNCTION carac

END MODULE module_reacteur_chimique
