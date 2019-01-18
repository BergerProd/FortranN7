!********************
SUBROUTINE read_data
!********************
!lecture du fichier d'entrée balistique.in
!les variables ici seront stockées en tant que variables globales
USE mod_balistique
IMPLICIT NONE
LOGICAL     :: file_exists

!vérification si fichier existant
INQUIRE(FILE='balistique.in',exist=file_exists)

IF (file_exists) THEN
    OPEN(1,FORM='FORMATTED',file='balistique.in')
    READ(1,*)
    READ(1,*)
    READ(1,*)methode  ! pour choisir entre RK4 et Euler
    READ(1,*)modele   ! pour choisir entre Chute Libre et Propulsé
    READ(1,*)npt
    READ(1,*)temps
    READ(1,*)
    READ(1,*)section  ! m2
    READ(1,*)vit_init ! m/s
    READ(1,*)alpha    ! angle sur vitesse initiale en degrés
    READ(1,*)alt_init
    READ(1,*)masse    ! masse missile en kg
    READ(1,*)rho_air  ! kg/m3
    READ(1,*)cl       ! pour portance
    READ(1,*)cd       ! pour trainée
    CLOSE(1)
ELSE
    PRINT*,' le fichier balistique.in n''a pas été trouvé'
    STOP
END IF

END SUBROUTINE read_data

!***********************
SUBROUTINE maillage
!***********************
!maillage sur le temps de la simulation

USE mod_balistique
IMPLICIT NONE

dt = temps/(float(npt-1)) ! pas de temps calculé à partir du temps de simulation et le nombre de points
t(1) = 0.d0

DO i=2,npt
  t(i)= t(i-1)+dt !on incrémente le temps
END DO

END SUBROUTINE maillage

!*************************
SUBROUTINE initialisation
!*************************
!initialisation des valeurs de vitesses et positions que l'on utilisera dans tous les calculs
!ces variables sont toutes globales
  USE mod_balistique
  IMPLICIT NONE

  alpha = (4.d0*datan(1.d0)/180)*alpha ! conversion de alpha en radian
  indice_le_zero=1 !indice utile pour obtenir les données à z = 0
  y(1,1) = 0.d0 !x(1)
  y(2,1) = alt_init !z(1)
  y(3,1) = vit_init*dcos(alpha) !vx(1)
  y(4,1) = vit_init*dsin(alpha) !vz(1)

END SUBROUTINE initialisation

!*****************************
SUBROUTINE solution_analytique
!*****************************
!Calculs des trajectoires selon les solutions analytiques pour le modèle Chute Libre
USE mod_balistique
IMPLICIT NONE

DO i=1,npt
!équations du mouvement d'une chute libre
    x_analt(i) = vit_init*t(i)*dcos(alpha)
    z_analt(i) = vit_init*t(i)*dsin(alpha) - (g/2.d0)*(t(i)**2) + alt_init

END DO

! Parametrisation Alpha pour obtenir portée et temps selon les équations analytiques

j=1 !indice j pour les différents alpha
DO i=25,75,5 ! Incrément de 25° à 75° par pas de 5°, i prendra les angles de 25 à 75
  ! calcul de la portée et du temps selon les solutions solutions_analytiques
  alpha_grad_analy(j) = i* (4.d0*datan(1.d0)/180) ! conversion de alpha (°) en (rad)
  portee_grad_analy(j) = (vit_init/g) * dcos(alpha_grad_analy(j))* (vit_init*dsin(alpha_grad_analy(j))+ (((vit_init*dsin(alpha_grad_analy(j)))**2) + 2*g*alt_init)**0.5)
  tl_grad_analy(j) = portee_grad_analy(j) /(vit_init * DCOS(alpha_grad_analy(j)))
  alpha_grad_analy(j) = i ! on affichera alpha en degrés en sortie
  j=j+1 !on incrémente j pour changer de ligne

END DO

END SUBROUTINE solution_analytique

!****************************
SUBROUTINE chute_libre_euler
!****************************
!Calcul de vitesses et positions avec methode d'Euler dans le modèle Chute Libre

USE mod_balistique
IMPLICIT NONE

DO i=1,npt-1

  y(3,i+1) = y(3,i)                !x(i+1)
  y(4,i+1) = y(4,i)  - dt*g        !z(i+1)
  y(1,i+1) = y(1,i)  + dt*y(3,i)   !vx(i+1)
  y(2,i+1) = y(2,i)  + dt*y(4,i)   !vz(i+1)

  !Pour affichage de la portée
  IF( (y(2,i) .LE. 0.d0) .AND. (indice_le_zero==1)) THEN ! ruse pour obtenir l'indice ou z <= 0
      indice_le_zero = i !on assigne le premier indice ou z<=0 à indice_le_zero que l'on récuperera dans le main.f90
  END IF

END DO

END SUBROUTINE chute_libre_euler


!*****************************
SUBROUTINE propulse_euler
!*****************************
!Calcul de vitesses et positions avec methode d'Euler dans le modèle Propulsé

USE mod_balistique
IMPLICIT NONE
REAL(KIND=8) :: theta,v,l,d !variables locales

DO i=1,npt-1

  v = sqrt(y(3,i)**2+y(4,i)**2) ! module de la vitesse
  theta = datan(y(4,i)/y(3,i)) ! argument de la vitesse

  l = cl*0.5d0*rho_air*section*(v**2) !calcul de la force de portance
  d = cd*0.5d0*rho_air*section*(v**2) !calcul de la force de trainée

  y(1,i+1) = y(1,i) + dt*y(3,i) !x(i+1)
  y(2,i+1) = y(2,i) + dt*y(4,i) ! z(i+1)
  y(3,i+1) = y(3,i) + dt*(0.7d0*g*dcos(theta) + (-l*dsin(theta)-d*dcos(theta))/masse) !vx(i+1)
  y(4,i+1) = y(4,i) + dt*(-g + 0.7d0*g*dsin(theta) + (l*dcos(theta) - d*dsin(theta))/masse) !vz(i+1)

    !Pour affichage de la portée
    IF( (y(2,i) .LE. 0.d0) .AND. (indice_le_zero==1)) THEN ! ruse pour obtenir l'indice ou z <= 0
      indice_le_zero = i !on assigne le premier indice ou z<=0 à indice_le_zero que l'on récuperera dans le main.f90
    END IF

END DO

END SUBROUTINE propulse_euler

!*****************************
SUBROUTINE rk4(derivee,n)
!*****************************
!Subroutine de la méthode rk4 que l'on appelle quand on utilise la méthode RK4
!Input : dérivée de la fonction, indice du vecteur utilisé
!y est une matrice globale donc on la modifie en globale directement dans cette subroutine

USE mod_balistique
IMPLICIT NONE
REAL(KIND=8),INTENT(IN)       :: derivee !de la fonction
INTEGER,INTENT(IN)            :: n !vecteur que l'on cherche à calculer, prend 1,2,3 ou 4
REAL(KIND=8)                  :: k1,k2,k3,k4 !variables locales pour RK4
! équations de rk4
k1 = derivee
k2 = derivee + (dt/2.d0) * k1
k3 = derivee + (dt/2.d0) * k2
k4 = derivee + dt * k3
y(n,i+1) = y(n,i) + (dt/6.d0)*(k1+2.d0*k2+2.d0*k3+k4)

END SUBROUTINE rk4

!*************************
SUBROUTINE chute_libre_rk4
!*************************
!Calcul de vitesses et positions avec methode RK4 dans le modèle Chute Libre

USE mod_balistique
IMPLICIT NONE
DO i=1,npt-1
  !on appelle la subroutine rk4 pour chaque vecteur
  CALL rk4(0.d0,3)   !x(i+1)
  CALL rk4(-g,4)     !z(i+1)
  CALL rk4(y(4,i),2) !vx(i+1)
  CALL rk4(y(3,i),1) !vz(i+1)

  !Pour affichage de la portée
  IF( (y(2,i) .LE. 0.d0) .AND. (indice_le_zero==1)) THEN ! ruse pour obtenir l'indice ou z <= 0
      indice_le_zero = i !on assigne le premier indice ou z<=0
  END IF
END DO

END SUBROUTINE chute_libre_rk4


!***************************
SUBROUTINE propulse_rk4
!***************************
!Calcul de vitesses et positions avec methode RK4 dans le modèle Propulsé

USE mod_balistique
IMPLICIT NONE

REAL(KIND=8) :: theta,v,l,d !variables locales

DO i=1,npt-1

  v = sqrt(y(3,i)**2+y(4,i)**2) ! module de la vitesse
  theta = datan(y(4,i)/y(3,i)) ! argument de la vitesse

  l = cl*0.5d0*rho_air*section*(v**2) !calcul de la force de portance
  d = cd*0.5d0*rho_air*section*(v**2) !calcul de la force de trainée

  CALL rk4(0.7d0*g*dcos(theta) + (-l*dsin(theta)-d*dcos(theta))/masse,3) !x(i+1)
  CALL rk4(-g + 0.7d0*g*dsin(theta) + (l*dcos(theta) - d*dsin(theta))/masse,4)!z(i+1)
  CALL rk4(y(4,i),2) !vz(i+1)
  CALL rk4(y(3,i),1) !vx(i+1)

  !Pour affichage de la portée
  IF( (y(2,i) .LE. 0.d0) .AND.(indice_le_zero==1)) THEN ! ruse pour obtenir l'indice ou z <= 0
    indice_le_zero = i!on assigne le premier indice ou z<=0
  END IF
END DO

END SUBROUTINE propulse_rk4


!*******************************
SUBROUTINE parametrisation_alpha
!*******************************
USE mod_balistique
IMPLICIT  NONE
INTEGER :: k,l
l=1
DO k=25,75,5 ! on fait varier l'angle de 25 à 75 par pas de 5
  alpha = k
  ! on reprend le menu du programme pricipal
  CALL initialisation()
  SELECT CASE(methode)
    CASE(1)
      !euler
      SELECT CASE (modele)
        CASE(1)
         ! chute libre
         CALL chute_libre_euler()
        CASE(2)
         ! propulsé
         CALL propulse_euler()
        CASE DEFAULT
      END SELECT

    CASE(2)
        SELECT CASE (modele)
          CASE(1)
            ! chute libre
            CALL chute_libre_rk4()
          CASE(2)
            ! propulsé
             CALL propulse_rk4()
          CASE DEFAULT
          END SELECT

    CASE DEFAULT
  END SELECT

  portee_grad(l) = y(1,indice_le_zero) !Portée du missile
  tl_grad(l) = t(indice_le_zero) !temps de vol
  alpha_grad(l) = k ! on reconverti alpha en degré
  l=l+1

END DO

END SUBROUTINE parametrisation_alpha

!*************************
SUBROUTINE affichage_sortie
!****************************
!Affichage des différents fichiers de sortie

USE mod_balistique
IMPLICIT NONE
CHARACTER(LEN=50)            :: nom
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

!Affichage des trajectoires pour alpha choisi

nom='BE_'//trim(tmp1)//'_'//trim(tmp2)//'_npt_'//carac(5,npt)//'.out'
! trim permet de supprimer les espaces de fin lorsqu'on a des espaces inutilisés

PRINT*, 'Regardez dans le fichier ',nom
OPEN(1,FORM='FORMATTED',FILE=TRIM(nom))

IF (modele==1) THEN !modèle chute libre on fait sortir les solutions analytiques

  WRITE(1,100)
  100 FORMAT('#',2x,'i',2x,'t',15x,'x',15x,'z',15x,'x_ana',15x,'z_ana')
 !les 15x correspondent aux espaces entre chaque colonnes pour aligner le nom avec les valeurs dans la colonnes

 DO i=1,npt
      WRITE(1,200)i,t(i),y(1,i),y(2,i),x_analt(i),z_analt(i) !on peut mettre les analytiques avec chute libre uniquement
  END DO

  200 FORMAT(i5,5(e13.6,3x)) !formattage pour les valeurs dans les colonnes

ELSEIF(modele==2)THEN !modèle propulse, on ne fait pas sortir les solutions analytiques

    WRITE(1,300)
    300 FORMAT('#',2x,'i',2x,'t',15x,'x',15x,'z',15x,'v_x',15x,'v_z')

   DO i=1,npt
        WRITE(1,400)i,t(i),y(1,i),y(2,i),y(3,i),y(4,i) !dans le modèle propulsé on affiche x,z,v_x,v_z
    END DO

    400 FORMAT(i5,5(e13.6,3x)) !formattage pour les valeurs dans les colonnes
END IF

CLOSE(1)

END SUBROUTINE affichage_sortie

!************************
SUBROUTINE affichage_parametrisation_alpha
!************************

!Affichage des Portées et temps pour les différents alpha

USE mod_balistique
IMPLICIT NONE
CHARACTER(LEN=50)            :: nom
CHARACTER(LEN=11)            :: tmp1,tmp2


nom = 'Paramétrisation_Alpha_'//trim(tmp1)//'_'//trim(tmp2)//'.out'
PRINT*, 'Regardez dans le fichier ', nom

OPEN(1,FORM='FORMATTED',FILE=TRIM(nom)) !ouverture

IF (modele==1) THEN ! si on fait chute libre alors on compare aux valeurs analytiques

  WRITE(1,500)
  500 FORMAT('#i',2x,"alpha (°)",6x,"Portee_ana(m)",3x,"Temps_ana l(s)",4x "Portee(m)",6x,"Temps_l(s)")

  DO i=1,11
    WRITE(1,600) i,alpha_grad_analy(i),portee_grad_analy(i),tl_grad_analy(i),portee_grad(i),tl_grad(i)
  END DO

  600 FORMAT(i2,5(e13.6,3x))

ELSE ! sinon on affiche juste les portées et les temps

  WRITE(1,700)
  700 FORMAT('#i',2x,"alpha (°)",7x,"Portee(m)",6x,"Temps(s)")

  DO i=1,11
    WRITE(1,800) i,alpha_grad(i),portee_grad(i),tl_grad(i)
  END DO

  800 FORMAT(i2,3(e13.6,3x))

END IF

CLOSE(1)!fermeture fichier

END SUBROUTINE affichage_parametrisation_alpha
