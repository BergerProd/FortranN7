!********************
SUBROUTINE read_data
!********************

USE mod_balistique
IMPLICIT NONE
LOGICAL     :: file_exists

INQUIRE(FILE='balistique.in',exist=file_exists)

IF (file_exists) THEN
    OPEN(1,FORM='FORMATTED',file='balistique.in')
    READ(1,*)
    READ(1,*)
    READ(1,*)methode
    READ(1,*)modele
    READ(1,*)npt
    READ(1,*)temps
    READ(1,*)
    READ(1,*)section !m2
    READ(1,*)vit_init !m/s
    READ(1,*)alpha !angle sur vitesse initiale en degrés
    READ(1,*)alt_init
    READ(1,*)masse !masse missile en kg
    READ(1,*)rho_air !kg/m3
    READ(1,*)cl !pour portance
    READ(1,*)cd ! pour trainée
    CLOSE(1)
ELSE
    PRINT*,' le fichier balistique.in n''a pas été trouvé'
    STOP
END IF

alpha = (4.d0*datan(1.d0)/180)*alpha ! conversion de alpha en radian mettre dans main ?

END SUBROUTINE read_data

!***********************
SUBROUTINE maillage
!***********************
USE mod_balistique
IMPLICIT NONE

dt = temps/(float(npt-1))
t(1) = 0.d0

DO i=2,npt
  t(i)= t(i-1)+dt
END DO

END SUBROUTINE maillage

!**************************
SUBROUTINE solution_analytique
!*****************************
USE mod_balistique
IMPLICIT NONE

DO i=1,npt
  !faire une condition pour stopper quand on est à z=0
    x_analt(i) = vit_init*t(i)*dcos(alpha)
    z_analt(i) = vit_init*t(i)*dsin(alpha) - (g/2.d0)*(t(i)**2) + alt_init

END DO

END SUBROUTINE solution_analytique

!****************************
SUBROUTINE chute_libre_euler
!****************************

USE mod_balistique
IMPLICIT NONE
REAL(KIND=8) :: portee_max,t_l
INTEGER :: j

y(1,1) = 0.d0 !x(1)
y(2,1) = alt_init !z(1)

DO i=1,npt

  y(3,i) = vit_init*dcos(alpha)
  y(4,i) = vit_init*dsin(alpha) - g*t(i)
  y(1,i+1)= y(1,i)+ dt*y(3,i)
  y(2,i+1)= y(2,i)+ dt*y(4,i)

END DO

portee_max = (vit_init/g) * DCOS(alpha)* (vit_init*dsin(alpha)+ (((vit_init*DSIN(alpha))**2) + 2*g*alt_init)**0.5)
t_l = portee_max /(vit_init * DCOS(alpha))

WRITE(*,*) "la portée maximum L = " ,portee_max !formater affichage
WRITE(*,*) "le temps associé à la portée maximale est tl = ",t_l

!calcul de la portée et de tl pour chaque angle initial par pas de 5 degrés
j=1
DO i=25,75,5 !pas de 5 pour la paramétrisation de alpha

  alpha_grad(j) = i* (4.d0*datan(1.d0)/180) !on passe alpha_grad en radian
  portee(j) = (vit_init/g) * DCOS(alpha_grad(j))* (vit_init*dsin(alpha_grad(j))+ (((vit_init*DSIN(alpha_grad(j)))**2) + 2*g*alt_init)**0.5)
  tl(j) = portee(j) /(vit_init * DCOS(alpha_grad(j)))
  j=j+1

END DO

alpha_grad = (/25,30,35,40,45,50,55,60,65,70,75/) ! on repasse alpha_grad en degrés


END SUBROUTINE chute_libre_euler

!*****************************
SUBROUTINE propulse_euler
!*****************************

USE mod_balistique
IMPLICIT NONE
REAL(KIND=8) :: theta,f_zero,v,l,d
INTEGER      :: j

j=1
f_zero = 0.7d0*masse*g
y(1,1) = 0.d0 !x(1)
y(2,1) = alt_init !z(1)
y(3,1) = vit_init*dcos(alpha)
y(4,1) = vit_init*dsin(alpha)
theta = alpha


DO i=2,npt
  v = sqrt(y(3,i-1)**2+y(4,i-1)**2)
  theta = datan(y(4,i-1)/y(3,i-1))
! indice i-1 puisqu'on doit utiliser vitesses de l'itération devant puisque les actuelles ne sont pas encore calculées
  l = cl*0.5d0*rho_air*section*(v**2)
  d = cd*0.5d0*rho_air*section*(v**2)


  y(3,i) = vit_init*dcos(alpha) + t(i)*(f_zero*dcos(theta)-l*dsin(theta)-l*dcos(theta))/masse
  y(4,i) = vit_init*dsin(alpha) + t(i)*(-g*masse + f_zero *dsin(theta)+ l*dcos(theta) - d*dsin(theta))/masse
  y(1,i)= y(1,i-1)+ dt*y(3,i)
  y(2,i)= y(2,i-1)+ dt*y(4,i)

  IF( (y(2,i) .LE. 0.05d0) .AND. (y(2,i) .GE. 0d0) .AND.(j==1)) THEN ! ruse pour obtenir l'indice ou z <= 0
      j = i
    END IF
END DO

  WRITE(*,*) "la portée maximum L = " , y(1,j) !formater affichage
  WRITE(*,*) "le temps associé à la portée maximale est tl = ", t(j)

END SUBROUTINE propulse_euler

!*************************
SUBROUTINE chute_libre_rk4
!*************************

USE mod_balistique
IMPLICIT NONE
REAL(KIND=8) :: portee_max,t_l,k1,k2,k3,k4


y(1,1) = 0.d0 !x(1)
y(2,1) = alt_init !z(1)

DO i=1,npt

  ! vitesse en x
  y(3,i) = vit_init*dcos(alpha)
  k1 = y(3,i)!x
  k2 = y(3,i) + (dt/2.d0) * k1 ! dt * vitesse
  k3 = y(3,i) + (dt/2.d0) * k2
  k4 = y(3,i) + dt * k3 ! j'ai mis y(3,i) à la place de y(1,i)
  ! position en x
  y(1,i+1)= y(1,i)+ (dt/6.d0)*(k1+2.d0*k2+2.d0*k3+k4)

  ! vitesse en z
  y(4,i) = vit_init*dsin(alpha) - g*t(i)
  k1 = y(4,i)!x
  k2 = y(4,i) + (dt/2.d0) * k1 ! dt * vitesse
  k3 = y(4,i) + (dt/2.d0) * k2
  k4 = y(4,i) + dt * k3

  y(2,i+1)= y(2,i)+ (dt/6.d0)*(k1+2.d0*k2+2.d0*k3+k4)

  portee_max = (vit_init/g) * DCOS(alpha)* (vit_init*dsin(alpha)+ (((vit_init*DSIN(alpha))**2) + 2*g*alt_init)**0.5)
  t_l = portee_max /(vit_init * DCOS(alpha))

  WRITE(*,*) "la portée maximum L = " ,portee_max !formater affichage
  WRITE(*,*) "le temps associé à la portée maximale est tl = ",t_l

END DO

END SUBROUTINE chute_libre_rk4

!***************************
SUBROUTINE propulse_rk4
!***************************

USE mod_balistique
IMPLICIT NONE

REAL(KIND=8) :: k1,k2,k3,k4,theta,f_zero,v,l,d
INTEGER      :: j

j=1
f_zero = 0.7d0*masse*g
y(1,1) = 0.d0 !x(1)
y(2,1) = alt_init !z(1)
y(3,1) = vit_init*dcos(alpha)
y(4,1) = vit_init*dsin(alpha)
theta = alpha

DO i=2,npt
  v = sqrt(y(3,i-1)**2+y(4,i-1)**2)
  theta = datan(y(4,i-1)/y(3,i-1))


  l = cl*0.5d0*rho_air*section*(v**2)
  d = cd*0.5d0*rho_air*section*(v**2)

  y(3,i) = vit_init*dcos(alpha) + t(i)*(f_zero*dcos(theta)-l*dsin(theta)-l*dcos(theta))/masse

  k1 = y(3,i)!x
  k2 = y(3,i) + (dt/2.d0) * k1
  k3 = y(3,i) + (dt/2.d0) * k2
  k4 = y(3,i) + dt * k3

  y(1,i)= y(1,i-1)+ (dt/6.d0)*(k1+2.d0*k2+2.d0*k3+k4)

  y(4,i) = vit_init*dsin(alpha) + t(i)*(-g*masse + f_zero *dsin(theta)+ l*dcos(theta) - d*dsin(theta))/masse

  k1 = y(4,i)!x
  k2 = y(4,i) + (dt/2.d0) * k1
  k3 = y(4,i) + (dt/2.d0) * k2
  k4 = y(4,i) + dt * k3

  y(2,i)= y(2,i-1)+ (dt/6.d0)*(k1+2.d0*k2+2.d0*k3+k4)

  IF( (y(2,i) .LE. 0.05d0) .AND. (y(2,i) .GE. 0d0) .AND.(j==1)) THEN ! ruse pour obtenir l'indice ou z <= 0
      j = i
    END IF
END DO

  WRITE(*,*) "la portée maximum L = " , y(1,j) !formater affichage
  WRITE(*,*) "le temps associé à la portée maximale est tl = ", t(j)


END SUBROUTINE propulse_rk4

!*************************
SUBROUTINE affichage_sortie
!****************************

USE mod_balistique
IMPLICIT NONE
CHARACTER(LEN=40)            :: nom
CHARACTER(LEN=11)            :: tmp1,tmp2


IF (methode==1) THEN
  tmp1 = "Euler"
ELSE
  tmp1 = "RK4"
ENDIF

IF(modele==1) THEN
  tmp2 = "Chute_Libre"
ELSE
  tmp2 = "Propulsé"
ENDIF

nom='BE_'//trim(tmp1)//'_'//trim(tmp2)//'_npt_'//carac(5,npt)//'.out'

! trim permet de supprimer les espaces de fin lorsqu'on a des espaces inutilisés

PRINT*, 'Regardez dans le fichier ',nom

OPEN(1,FORM='FORMATTED',FILE=TRIM(nom))

WRITE(1,100)
100 FORMAT('#',2x,'i',2x,'t',15x,'x',15x,'z',15x,'x_ana',15x,'z_ana')
 !les 15x correspondent aux espaces entre chaque colonnes pour aligner le nom avec les valeurs dans la colonnes

DO i=1,npt
    WRITE(1,200)i,t(i),y(1,i),y(2,i),x_analt(i),z_analt(i)
END DO

200 FORMAT(i5,8(e13.6,3x)) !formattage pour les valeurs dans les colonnes
CLOSE(1)


IF ((modele==1) .AND. (methode==1)) THEN ! si on est dans le cas chute libre on affiche Parametrisation_Alpha
nom = "Parametrisation_Alpha"

PRINT*, "Regardez dans le fichier ", nom

OPEN(1,FORM='FORMATTED',FILE=TRIM(nom))

WRITE(1,300)
300 FORMAT('#',2x,'i',2x,"alpha (°)",15x,"Portee (m)",15x,"Temps L (s)")

DO i=1,11
  WRITE(1,400) i, alpha_grad(i),portee(i),tl(i)
END DO

400 FORMAT(i5,3(e13.6,3x))

CLOSE(1)

END IF

END SUBROUTINE affichage_sortie
