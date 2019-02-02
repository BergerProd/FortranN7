!********************
SUBROUTINE read_data
!********************

use module_tp3
IMPLICIT NONE
LOGICAL     :: file_exists

! regarder El'utilisation de INQUIRE
INQUIRE(FILE='tp3.in',exist=file_exists)

IF (file_exists) THEN
    OPEN(1,FORM='FORMATTED',file='tp3.in')
    READ(1,*)
    READ(1,*)option
    READ(1,*)x1
    READ(1,*)x2
    READ(1,*)npt
    CLOSE(1)
ELSE
    PRINT*,' le fichier tp3.in n''a pas été trouvé'
    STOP
END IF

END SUBROUTINE read_data



!*******************
SUBROUTINE maillage
!******************

use module_tp3
IMPLICIT NONE

INTEGER         :: i

! Maillage régulier (je pourrais faire ce que je veux)
dx=(x2-x1)/DFLOAT(npt-1)
x(1)=x1;
DO i=2,npt
    x(i)=x(i-1)+dx
END DO

END SUBROUTINE maillage

!****************************
SUBROUTINE calcul_fonctions
!****************************
use module_tp3
IMPLICIT NONE
REAL(KIND=8)        ::c1,c2,c3
REAL(KIND=8),PARAMETER   ::deux=2.D0

SELECT CASE (option)
    CASE(1)
        y(:)=COS(x(:))*SIN(x(:))**2
        dy_analy(:)=-SIN(x(:))**3+2.D0*COS(x(:))**2*SIN(x(:))

    CASE(2)
        c1=2.d0; c2=0.5d0
        y(:)=c1*x(:)+c2*x(:)**2
        dy_analy(:)=c1+deux*c2*x(:)

    CASE(3)
        c1=2.d0; c2=-0.05d0; c3=3.d0
        y(:)=c1*EXP(c2*x(:))*COS(c3*x(:))
        dy_analy(:)= c1*EXP(c2*x(:))*(           &
            c2*COS(c3*x(:))-c3*SIN(c3*x(:)) &
        )
    CASE DEFAULT
        y(:)=x(:)
        dy_analy(:)=1.d0
END SELECT


END SUBROUTINE calcul_fonctions


!****************************
SUBROUTINE derivee_numerique
!****************************

USE module_tp3
IMPLICIT NONE
INTEGER         :: i
! ordre 2 partout sauf sur  les bords, attention pas constant

DO i=2,npt-1
    dy_num(i)=0.5d0*(y(i+1)-y(i-1))
END DO
dy_num(1)  = 0.5d0*(-y(3)     + 4.d0*y(2)     - 3.d0*y(1)    )
dy_num(npt)= 0.5d0*( y(npt-2) - 4.d0*y(npt-1) + 3.d0*y(npt)  )


dy_num(:)=dy_num(:)/dx

END SUBROUTINE derivee_numerique

!****************************
SUBROUTINE calcul_erreur
!****************************

USE module_tp3
IMPLICIT NONE
REAL(KIND=8),PARAMETER          :: epsi=1.d-13
INTEGER                         :: i
INTEGER,DIMENSION(1)            :: k

DO i=1,npt
    IF (abs(dy_analy(i)).GT.epsi) THEN
        erreur(i)=abs(1.d0-dy_num(i)/dy_analy(i))
    ELSE
        PRINT*, 'cas dérivée nulle' ,dy_analy(i)
        !erreur(i)=abs(1.d0-dy_num(i)/epsi)
        erreur(i)=epsi
    END IF

    ! erreur(i)=abs(1.d0-dy_num(i)/max(epsi,dy_analy(i)))

END DO
erreur_max=maxval(erreur)
k         =maxloc(erreur)
i_max     =k(1)
x_max     =x(i_max)

END SUBROUTINE calcul_erreur



!****************************
SUBROUTINE affichage_sortie
!****************************

USE module_tp3
IMPLICIT NONE
CHARACTER(LEN=30)           :: nom
INTEGER                     :: i
REAL(KIND=8),EXTERNAL       :: fLOG10
CHARACTER(LEN=5)            :: tmp

! je met un nom de fichier de sortie dont le nom indique le choix de la fonction
! et le nombre de point

! je suis obligé de ruser avec les longueurs des chaines de caracteres
! car je suis obligé de définir carac d'une longueur constante
tmp=carac(5,option)
nom='tp3_cas_'//tmp(5:5)//'_npt_'//carac(5,npt)//'.out'

PRINT*,'pour la fonction ',option
PRINT*, 'L''erreur maximale sur la derivee est :'
PRINT*, 'max (erreur) = ',erreur_max,' a  x(',i_max,') = ',x_max
PRINT*, 'Regarder dans le fichier ',nom

OPEN(1,FORM='FORMATTED',FILE=TRIM(nom))
WRITE(1,100)
100 FORMAT('#',2x,'i',8x,'x',8x,'y',8x,'dy_analy',6x,'dy_num',6x, &
         'erreur',8x, 'log10(erreur)')

DO i=1,npt
    WRITE(1,200)i,x(i),y(i),dy_analy(i),dy_num(i),erreur(i),fLOG10(erreur(i))
END DO

200 FORMAT(i4,2x,7(e13.6,3x))
CLOSE(1)

END SUBROUTINE affichage_sortie

!*******************
FUNCTION fLOG10(x)
!*******************
IMPLICIT NONE
REAL(KIND=8),INTENT(IN)     :: x
REAL(KIND=8)                :: flog10
REAL(KIND=8),PARAMETER      :: fmin=-20.d0,epsil=1.d-20

IF (x.lt.epsil) THEN
    fLOG10=fmin
ELSE
    fLOG10=LOG10(x)
END IF
END FUNCTION fLOG10
