PROGRAM main

!TODO normalement on devrait allouer xcentre_vol,ycentre_vol,xcentre_faces_horiz,ycentre_faces_horiz en taille n-1*n-1
!Comment afficher tout ça sans avoir d'erreur ?
!TODO Calcul du pas de temps

USE module_reacteur_chimique
IMPLICIT NONE

pi = 4.d0*DATAN(1.d0)

CALL read_data()
!Normalement xcentre_vol et ycentre_vol sont de taille n-1, mais on les allocate à n pour ne pas se faire jeter quand on affiche tout en sortie
ALLOCATE(xnoeuds(nptx,npty),ynoeuds(nptx,npty), &
xcentre_vol(nptx-1,npty-1),ycentre_vol(nptx-1,npty-1), &
xcentre_faces_horiz(nptx,npty),ycentre_faces_horiz(nptx,npty),&
xcentre_faces_vertic(nptx,npty),ycentre_faces_vertic(nptx,nptx),&
ux_centres_vol(nptx-1,npty-1),uy_centres_vol(nptx-1,nptx-1),temperature(nptx-1,npty-1),&
ux_centres_faces(nptx-1,npty-1),uy_centres_faces(nptx-1,nptx-1),&
flux_adv_bas(nptx,npty),flux_adv_haut(nptx,npty),flux_adv_gauche(nptx,npty),flux_adv_droit(nptx,npty),&
flux_tot(nptx,npty))


CALL maillage()
CALL champ_vitesse()
CALL champ_temperature()
CALL calcul_flux_advectif()
!CALL affichage_sortie()

CALL VTSWriter(0,0,nptx,npty,xnoeuds,ynoeuds,temperature,ux_centres_vol,uy_centres_vol,'ini')
DEALLOCATE(xnoeuds,ynoeuds,xcentre_vol,ycentre_vol,xcentre_faces_horiz,ycentre_faces_horiz,&
xcentre_faces_vertic,ycentre_faces_vertic,temperature,flux_adv_gauche,flux_adv_haut,flux_adv_droit,flux_adv_bas,&
ux_centres_vol,ux_centres_faces,uy_centres_faces,uy_centres_vol,flux_tot)


END PROGRAM main
