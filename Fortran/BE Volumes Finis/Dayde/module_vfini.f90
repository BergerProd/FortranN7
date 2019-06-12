!**********************************************************************
!*************************BE VOLUMES FINIS*****************************
!************************DAYDE-THOMAS / LALU***************************
!*****************************MODULE***********************************
!**********************************************************************

MODULE module_vfini

IMPLICIT NONE 

REAL(KIND=4)                                        :: L,A,deltax,deltay,alpha,intervalles,tfinal,dt
REAL(KIND=4)                                        :: pi=4.*ATAN(1.),U0=1.,V0=1.
INTEGER                                             :: nptx,npty
REAL(KIND=4),ALLOCATABLE,DIMENSION(:,:)             :: U,V,x,y,xm,ym,T
REAL(KIND=4),ALLOCATABLE,DIMENSION(:,:)             :: Fd,Fg,Fh,Fb,Ftot,advection
!REAL(KIND=4),ALLOCATABLE,DIMENSION(:,:)             :: Ta,Tb       !1D

END MODULE module_vfini

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
END IF

n=N_in-1
i=i_enter
DO k=n,0,-1
        carac(n-k+1:n-k+1)=CHAR(48+INT(i/10**k))
        i=MOD(i,10**k)
END DO
END FUNCTION carac
