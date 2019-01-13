PROGRAM main
USE mod_balistique
IMPLICIT NONE

CALL read_data()
ALLOCATE(y(4,npt),x_analt(npt),z_analt(npt),t(npt),portee(10),tl(10),alpha_grad(10))
CALL maillage()

SELECT CASE(methode)
CASE(1)
    !euler
    SELECT CASE (modele)
      CASE(1)
       ! chute libre
       CALL chute_libre_euler()
       CALL solution_analytique()
       !CALL parametrisation_alpha
      CASE(2)
       ! propulsé
       CALL propulse_euler()
      CASE DEFAULT
      WRITE(*,*) " Modèle doit prendre comme argument 1 ou 2, Erreur dans fichier d'entrée ligne 4"
    END SELECT

CASE(2)
        SELECT CASE (modele)
         CASE(1)
           ! chute libre
            CALL chute_libre_rk4()
           CALL solution_analytique()
           !CALL parametrisation_alpha
         CASE(2)
           ! propulsé
           CALL propulse_rk4()
         CASE DEFAULT
          WRITE(*,*) " Modèle doit prendre comme argument 1 ou 2, Erreur dans fichier d'entrée ligne 4"

        END SELECT
      CASE DEFAULT
          WRITE(*,*) " Méthode doit prendre comme argument 1 ou 2, Erreur dans fichier d'entrée ligne 3"
END SELECT

CALL affichage_sortie()

!TODO formater affichage à l'écran
!TODO est ce qu'on ferait pas uniquement si on utilise la chute libre on affiche les solutions_analytiques ?
!TODO  centraliser l'affichage_sortie pour afficher tout dans cette subroutine ?
!TODO revoir les variables globales puisque certaines ne doivent pas être utilisées ?
!TODO  est ce qu'on ne ferait pas paramétrisation alpha comme une subroutine pour l'appeler à part ?
!TODO afficher parmetrisation alpha uniquement lorsque Chute_Libre !Parametrisation_Alpha ********* mettre pour méthode 2 aussi ?
!TODO faire sortir v_x et v_z ?
!TODO faire la conversion d'alpha ailleurs que dans sortie ?
!TODO faire sortit write juste une fois dans le main
!TODO subroutine initialisation avec :
!y(1,1) = 0.d0 !x(1)
!y(2,1) = alt_init !z(1)
!f_zero = 0.7d0*masse*g !calcul de la force
!alpha = (4.d0*datan(1.d0)/180)*alpha ! conversion de alpha en radian
!TODO faire paramétrisation alpha pour cas propulse

DEALLOCATE(t,x_analt,z_analt,y,portee,tl,alpha_grad)
END PROGRAM main
