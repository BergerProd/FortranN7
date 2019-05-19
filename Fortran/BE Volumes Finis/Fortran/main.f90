PROGRAM main

!TODO faces
!TODO champ_vitesse avec le matrciel et à savoir si centre face ou noeuds ?
!TODO enlever ux,uy
!TODO bizarre sur vitesses en x premier négatif alors qu'il devrait etre positif
!TODO normalement on devrait allouer xcentre_vol,ycentre_vol,xcentre_faces_horiz,ycentre_faces_horiz en taille n-1*n-1
!Comment afficher tout ça sans avoir d'erreur ? 

USE module_reacteur_chimique
IMPLICIT NONE

pi = 4.d0*DATAN(1.d0)

CALL read_data()
!Normalement xcentre_vol et ycentre_vol sont de taille n-1, mais on les allocate à n pour ne pas se faire jeter quand on affiche tout en sortie
ALLOCATE(xnoeuds(nptx,npty),ynoeuds(nptx,npty),xcentre_vol(nptx,npty),ycentre_vol(nptx,npty))
ALLOCATE(xcentre_faces_horiz(nptx,npty),ycentre_faces_horiz(nptx,nptx),xcentre_faces_vertic(nptx,npty),ycentre_faces_vertic(nptx,nptx),u(nptx,npty))
CALL maillage()
CALL champ_vitesse()
CALL affichage_sortie()

DEALLOCATE(xnoeuds,ynoeuds,xcentre_vol,ycentre_vol,xcentre_faces_horiz,ycentre_faces_horiz,xcentre_faces_vertic,ycentre_faces_vertic,u)

END PROGRAM main
