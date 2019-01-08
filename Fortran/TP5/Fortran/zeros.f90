!**********
MODULE mod
!**********
INTEGER                               :: option1,option2
END MODULE mod

! on va faire un maillage avec x , on va calculer f pour chaque x et on va faire f(x)*f(x+1)<0 si ça ça veut dire qu'on à un zéro
! on regarde lorsque qu'on à un 0 on récupere le x

!************
PROGRAM zeros
!************
USE mod
IMPLICIT NONE

ALLOCATE(x(npt),f_x(npt))

! faire une subroutine Menu ?
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
    ![0.5,1.5]
    CALL Newton(0.d0,0.5d0)
  ELSEIF(option2==2) THEN
    ![0.5,1.5]
    CALL Newton(0.d0,0.d0)
  ELSE
    WRITE(*,*)"Erreur pour intervalle, Option ne peut prendre que 1, 2"
  END IF

CASE(2)
  !f(x) = cos(x)
  CALL Newton(0.d0,1.57d0)
CASE(3)
  !f(x) = fanno
  CALL Newton(0.5d0,0.1d0)
CASE DEFAULT
  WRITE(*,*)"Option ne peut prendre que 1, 2 et 3"
END SELECT

DEALLOCATE(x,f_x)
END PROGRAM zeros



!************************
! FONCTIONS
!************************

!**************************
REAL(KIND=8)  FUNCTION f(s)! ne pas oublier de déclarer le type que renvoi la fonction
!**************************
USE mod
IMPLICIT NONE
REAL(KIND=8),INTENT(IN) :: s !déclarer en entrée s

SELECT CASE(option1) !option pour les différentes fonctions
CASE(1)
  f = s*(s-1)*(s-2)
CASE(2)
  f = dcos(s)
CASE(3)
  f = ((1.d0-s**2)/(1.4d0*s)) + ((1.4d0 + 1.d0)/(2.d0*1.4d0)) * LOG( ((1.4d0 + 1.d0)*(s**2))/(2/d0+ (1.4d0 - 1.d0*s)))
CASE DEFAULT
  WRITE(*,*) "Option doit prendre 1, 2 ou 3"
END SELECT

END FUNCTION f


!*****************************
!SUBROUTINES
!*****************************


!*************************************
SUBROUTINE Newton(valeur_cible,s_init)
!*************************************
!valeur_cible cest f(x)=valeur_cible
!s_init c'est la première valeur d'initialisation
USE mod
IMPLICIT NONE
REAL(KIND=8)   :: epsilon,erreur_max,s,gradient,variation,y,yeps,f !déclarer f ici
INTEGER        :: iter,iter_max
REAL(KIND=8),INTENT(IN)   :: valeur_cible,s_init

!paramètres
epsilon = 0.000000001d0
erreur_max = 0.00001d0
iter_max = 20

!initialisation des valeurs
s = s_init !
variation = 1.d0 !superieur à erreur_max
iter = 0
y = f(s) !première valeur de la fonction

DO WHILE ((abs(variation) .GT. erreur_max) .AND. (iter .LT. iter_max))
  iter = iter + 1
  yeps = f(s+epsilon)

  gradient = (yeps - y) / epsilon !dérivée
  variation = - (y - valeur_cible) / gradient
  s = s + variation
  y = f(s)

WRITE(*,*) "iteration en cours",iter," abscisse s",s,"variation",variation,"ordonnée y",y
  IF (iter == iter_max) THEN
  WRITE(*,*)"non convergence"
    STOP
  ELSE
  WRITE(*,*)"valeur trouvée = ",s
  ENDIF
END DO

END SUBROUTINE Newton
