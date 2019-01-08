PROGRAM tp4
! Calcul des profils NACA version 4 et 5 chiffres en utilisant le fichier profil.in en entrée
! Quentin Bergé MF2E 1A
! Utilisation d'un Makefile pour Compilation

USE module_tp4
IMPLICIT NONE

CALL read_data() ! lecture des données d'entrées
ALLOCATE (x(npt),xe(npt),xi(npt),f(npt),ye(npt),yi(npt),g(npt),h(npt),df(npt),theta(npt)) !allocation dynamique des variables globales
CALL maillage()

IF (option==4) THEN !cas NACA4
  CALL cambrure_NACA4()

ELSEIF (option==5) THEN
  IF (c==0) THEN
    CALL cambrure_simple_NACA5() !cas NACA5 avec cambrure simple
  ELSEIF (c==1) THEN
    CALL cambrure_double_NACA5() !cas NACA5 avec cambrure double
  ELSE
    WRITE(*,*) "Mauvaise saisie, Dans NACA5, C ne peut prendre que 2 valeurs 0 ou 1" !impossible que C prenne autre que 0 & 1
  ENDIF

ELSE   !impossible que option ne prenne autre que 4 & 5
  WRITE(*,*) "Mauvaise saisie, Ce programme est utilisé pour NACA 4 chiffres ou 5 chiffres uniquement"
ENDIF

CALL calcul_extrados_intrados()
CALL affichage_sortie()
DEALLOCATE(x,xe,xi,f,ye,yi,g,h,df,theta)

END PROGRAM tp4
