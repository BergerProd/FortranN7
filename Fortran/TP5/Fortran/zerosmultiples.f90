!**********
MODULE mod
!**********
INTEGER                               :: option1,option2 ! pour choix de cas
INTEGER,PARAMETER                     :: npt=200 ! nombre de points de discrétisation
REAL(KIND=8),DIMENSION(:),ALLOCATABLE :: x,fdex !x,f(x) pour la fonction utilisée
REAL(KIND=8)                          :: valeur_cible ! affectée selon la fonction utilisée
END MODULE mod

!************
PROGRAM zeros
!************
USE mod
IMPLICIT NONE

INTEGER :: i
ALLOCATE(x(npt),fdex(npt))

CALL menu()
CALL maillage()
CALL recherche_zeros()

DEALLOCATE(x,fdex)

END PROGRAM zeros

!************************
! FONCTIONS
!************************

!**************************
FUNCTION f(s)! ne pas oublier de déclarer le type que renvoi la fonction
!**************************
USE mod
IMPLICIT NONE
REAL(KIND=8),INTENT(IN) :: s !déclarer en entrée s
REAL(KIND=8)            :: f

SELECT CASE(option1) !option pour les différentes fonctions
CASE(1)
  f = s*(s-1.d0)*(s-2.d0)
CASE(2)
  f = dcos(s)
CASE(3)
  f = ((1.d0-s**2)/(1.4d0*s)) + ((1.4d0 + 1.d0)/(2.d0*1.4d0)) * LOG( ((1.4d0 + 1.d0)*(s**2))/(2.d0+ (1.4d0 - 1.d0*s)))
CASE DEFAULT
  WRITE(*,*) "Option doit prendre 1, 2 ou 3"
END SELECT

END FUNCTION f


!*****************************
!SUBROUTINES
!*****************************


!*****************************
SUBROUTINE menu
!*****************************

! Menu pour choix de la fonction
! affecte la valeur_cible tq f(x)=valeur_cible pour la fonction considérée
! affecte début et fin d'intervalle de x pour la fonction considérée

USE mod
IMPLICIT NONE
REAL(KIND=8) :: pi

pi = 4.d0*datan(1.d0)

WRITE(*,*)"Quels fonction voulez vous calculez ?"
WRITE(*,*)"Choix 1 : f(x) = x(x-1)(x-2)"
WRITE(*,*)"Choix 2 : f(x) = cos(x)"
WRITE(*,*)"Choix 3 : f(x) = fanno"
READ(*,*) option1

SELECT CASE(option1)

CASE(1)
  !f(x) = x(x-1)(x-2)
  WRITE(*,*)"Dans quel intervalle voulez vous calculer f(x) = x(x-1)(x-2) ?"
  WRITE(*,*)"Choix 1 : [0.5,1.5]"
  WRITE(*,*)"Choix 2 : [0,2]"
  READ(*,*) option2

  IF(option2==1) THEN
    !intervalle [0.5,1.5]
    x(1) = 0.5d0
    x(npt) = 1.5d0
    valeur_cible = 0.d0

  ELSEIF(option2==2) THEN
    ![0,2]
    x(1) = 0.d0
    x(npt) = 2.d0
    valeur_cible = 0.d0

  ELSE
    WRITE(*,*)"Erreur pour intervalle, Option ne peut prendre que 1, 2"
  END IF

CASE(2)
  !f(x) = cos(x)
  x(1) = 0.d0
  x(npt) = 2.d0*pi
  valeur_cible = 0.d0

CASE(3)
  !f(x) = fanno
  valeur_cible = 0.5d0
  x(1) = 0.1d0
  x(npt) = 1.d0
  valeur_cible = 0.5d0

CASE DEFAULT
  WRITE(*,*)"Option ne peut prendre que 1, 2 et 3"
END SELECT

END SUBROUTINE menu

!*******************
SUBROUTINE maillage
!******************
!maillage sur la longeur de la fonction
! on maille x et f(x)
USE mod
IMPLICIT NONE

INTEGER         :: i
REAL(KIND=8)    :: dx,f

! Maillage régulier sur x
dx=(x(npt)-x(1))/DFLOAT(npt-1)
DO i=2,npt
    x(i)=x(i-1)+dx
END DO

! Maillage régulier sur f(x), on calcul f(x) sur chaque x
DO i=1,npt
  fdex(i) = f(x(i))

END DO

END SUBROUTINE maillage

!*************************
SUBROUTINE recherche_zeros
!*************************
! D'abord on recherche les abcisses des zéros que l'on stocke dans le vecteur abcis_zero
! Pour cela on vérifie f(x)*f(x+1)<0, si c'est vrai alors on est passé par zéro
! On prend alors x que l'on affecte comme valeur de départ pour l'algorithme de Newton

USE mod
IMPLICIT NONE
REAL(KIND=8)                :: f !déclarer f ici puisqu'on l'utilise
INTEGER                     :: i

DO i = 1,npt-1
  IF((fdex(i)*fdex(i+1)) .LE. 0) THEN ! si produit negatif -> on passe par un zéro
    CALL Newton(valeur_cible,x(i))
    ! on appelle la SUBROUTINE Newton avec la valeur cible définie dans MENU
    ! avec la valeur de départ de l'abcisse par ou l'on passe pour avoir zéro
  END IF
END DO

END SUBROUTINE recherche_zeros

!*************************************
SUBROUTINE Newton(val_cible,s_init)
!*************************************
!valeur_cible cest f(x)=valeur_cible
!s_init c'est l'abcisse de départ
USE mod
IMPLICIT NONE
REAL(KIND=8)   :: epsilon,erreur_max,s,gradient,variation,y,yeps,f !déclarer f ici pour l'utiliser
INTEGER        :: iter,iter_max
REAL(KIND=8),INTENT(IN)   :: s_init,val_cible ! Arguments d'entrée

!paramètres
epsilon = 0.0000001d0
erreur_max = 0.00001d0
iter_max = 20

!initialisation des valeurs
s = s_init !
variation = 1.d0 !superieur à erreur_max
iter = 0
y = f(s) !première valeur de la fonction

DO WHILE ((abs(variation) .GT. erreur_max) .AND. (iter .LT. iter_max)) ! erreur de boucle
  iter = iter + 1 ! on incrémente
  yeps = f(s+epsilon) ! f(a+h)
  gradient = (yeps - y) / epsilon !dérivée : lim qd h tend vers 0 de (f(a+h) - f(a))/h
  variation = - (y - val_cible) / gradient
  s = s + variation
  y = f(s) !on affecte y à la prochaine valeur
!WRITE(*,*) "iteration en cours",iter," abscisse s",s,"variation",variation,"ordonnée y",y
IF (iter == iter_max) THEN ! on ne converge pas
  WRITE(*,*)"non convergence"
  STOP
ELSE
  WRITE(*,*)"valeur trouvée = ",s
ENDIF
END DO
WRITE(*,*) "Xzéro = ",s, "itération effectuées =",iter



END SUBROUTINE Newton
