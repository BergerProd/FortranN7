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
     CASE(2)
       ! propulsé
       CALL propulse_rk4()
    CASE DEFAULT
      WRITE(*,*) " Modèle doit prendre comme argument 1 ou 2, Erreur dans fichier d'entrée ligne 4"

    END SELECT
  CASE DEFAULT
      WRITE(*,*) " Méthode doit prendre comme argument 1 ou 2, Erreur dans fichier d'entrée ligne 3"
END SELECT

CALL solution_analytique()
CALL affichage_sortie()
!formatter affichage à l'écran
!faire une subroutine séparée pour la Paramétrisation_Alpha ?

DEALLOCATE(t,x_analt,z_analt,y,portee,tl,alpha_grad)
END PROGRAM main
