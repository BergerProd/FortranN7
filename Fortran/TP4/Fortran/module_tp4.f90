MODULE module_tp4

IMPLICIT NONE
!pour fichier entrée
INTEGER                               :: a,b,c,d,e !NACAabcd ou NACAabcde selon option
REAL(KIND=8)                          :: x1,x2
INTEGER                               :: npt,option
!traitement
REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: ye,yi,xe,xi,x,f,h,g,df,theta
REAL(KIND=8)                          :: l,t,p,m,eta !variables pouvant être déclarées dans les 3 subroutines de calcul de cambrure
REAL(KIND=8),PARAMETER                :: a0=0.2969d0,a1=0.1260d0,a2=0.3516d0,a3=0.2843d0,a4=0.1015d0 !paramètres pour g
!g demi-epaisseur
!f cambrure en réalité inutilisée
!xe,ye extrados
!xi,yi intrados
!x position le long de la corde


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

END MODULE
