PROGRAM main
  ! Calcul des trajectoires d'un objet en chute libre et en propulsé en utilisant Euler et RK4
  ! Quentin Bergé & Marc Ferrière MF2E 1A
  ! Utilisation d'un Makefile pour Compilation
  ! Fichier d'entrée : balistique.in, module : mod_balistique.f90
  ! Fichier de sortie : main.bin, Résultats sous la forme BE_méthode_modèle_npt.out & Paramétrisation_Alpha_Méthode_Modèle.out

USE mod_balistique
IMPLICIT NONE

CALL read_data()! lecture des données d'entrées
ALLOCATE(y(4,npt),x_analt(npt),z_analt(npt),t(npt),portee_grad_analy(11),tl_grad_analy(11),alpha_grad_analy(11),portee_grad(11),tl_grad(11),alpha_grad(11))
CALL maillage()
CALL initialisation()
 ! menu pour choisir la méthode et le modèle utilisé
SELECT CASE(methode)
CASE(1)
  !Euler
  SELECT CASE (modele)
    CASE(1)
    ! chute libre
      CALL chute_libre_euler()
      CALL solution_analytique()

    CASE(2)
    ! propulsé
       CALL propulse_euler()
    CASE DEFAULT
      WRITE(*,*) " Modèle doit prendre comme argument 1 ou 2, Erreur dans fichier d'entrée ligne 4"
    END SELECT

CASE(2)
    !RK4
    SELECT CASE (modele)
      CASE(1)
      ! chute libre
        CALL chute_libre_rk4()
        CALL solution_analytique()
      CASE(2)
      ! propulsé
        CALL propulse_rk4()
      CASE DEFAULT
        WRITE(*,*) " Modèle doit prendre comme argument 1 ou 2, Erreur dans fichier d'entrée ligne 4"
      END SELECT

      CASE DEFAULT
        WRITE(*,*) " Méthode doit prendre comme argument 1 ou 2, Erreur dans fichier d'entrée ligne 3"
END SELECT

!quand z n'arrive pas à 0, cela signifie que le temps de simulation est trop court
IF( abs(y(1,indice_le_zero) - 0) .LE. 1e-13) THEN
  WRITE(*,*) "Augmentez le temps de la simulation dans le fichier d'entrée, ligne 6"
END IF

!affichage de la portée et du temps ou z=0
WRITE(*,'(a,e14.5)') "la portée maximum L = " , y(1,indice_le_zero)
WRITE(*,'(a,e14.5)') "le temps associé à la portée maximale est tl = ", t(indice_le_zero)

CALL affichage_sortie()
CALL parametrisation_alpha()
CALL affichage_parametrisation_alpha()


DEALLOCATE(t,x_analt,z_analt,y,portee_grad_analy,tl_grad_analy,alpha_grad_analy,portee_grad,tl_grad,alpha_grad)
END PROGRAM main
