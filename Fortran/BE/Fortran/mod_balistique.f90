MODULE mod_balistique
REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:)  :: y
REAL(KIND=8),ALLOCATABLE,DIMENSION(:) :: t,x_analt,z_analt,portee,tl,alpha_grad
REAL(KIND=8)                          :: temps,section,vit_init,alt_init,masse,cl,cd,rho_air,alpha,dt
REAL(KIND=8),PARAMETER                :: g=9.81d0
INTEGER                               :: i,npt,methode,modele


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
INTEGER             :: k,n

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

END MODULE mod_balistique
