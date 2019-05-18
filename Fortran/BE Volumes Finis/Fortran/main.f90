PROGRAM main


!TODO faces
!TODO champ_vitesse avec le matrciel et à savoir si centre face ou noeuds ?

USE module_reacteur_chimique
IMPLICIT NONE

pi = 4.d0*DATAN(1.d0)

CALL read_data()
!Normalement xcentre_vol et ycentre_vol sont de taille n-1, mais on les allocate à n pour ne pas se faire jeter quand on affiche tout en sortie
ALLOCATE(xnoeuds(nptx,npty),ynoeuds(nptx,npty),xcentre_vol(nptx,npty),ycentre_vol(nptx,npty),ux(nptx,npty),uy(nptx,npty),u(nptx,npty))
CALL maillage()
!CALL champ_vitesse()
CALL affichage_sortie()

DEALLOCATE(xnoeuds,ynoeuds,xcentre_vol,ycentre_vol,ux,uy,u)

END PROGRAM main
