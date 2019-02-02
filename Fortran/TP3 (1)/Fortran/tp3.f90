PROGRAM tp3
! Programme principal à compléter
! Environ 13 lignes
USE module_tp3
CALL read_data()
!lire les données d'entrées
ALLOCATE (x(npt),y(npt),dy_num(npt),dy_analy(npt),erreur(npt))
!on alloue x et y selon la lectuer du nombre de points
CALL maillage()
CALL calcul_fonctions()
CALL derivee_numerique()
CALL calcul_erreur()
CALL affichage_sortie()
DEALLOCATE (x,y,dy_num,dy_analy,erreur)
END PROGRAM tp3
