PROGRAM main

!TODO normalement on devrait allouer xcentre_vol,ycentre_vol,xcentre_faces_horiz,ycentre_faces_horiz en taille n-1*n-1
!Comment afficher tout ça sans avoir d'erreur ?
!TODO Calcul du pas de temps
!TODO Temp doit enfait etre un (i,j,t) en taille

USE module_reacteur_chimique
IMPLICIT NONE
INTEGER :: k

pi = 4.d0*DATAN(1.d0)

CALL read_data()
!Normalement xcentre_vol et ycentre_vol sont de taille n-1, mais on les allocate à n pour ne pas se faire jeter quand on affiche tout en sortie
ALLOCATE(xnoeuds(nptx,npty),ynoeuds(nptx,npty), &
xcentre_vol(nptx-1,npty-1),ycentre_vol(nptx-1,npty-1), &
xcentre_faces_horiz(nptx,npty),ycentre_faces_horiz(nptx,npty),&
xcentre_faces_vertic(nptx,npty),ycentre_faces_vertic(nptx,nptx),&
ux_centres_vol(nptx-1,npty-1),uy_centres_vol(nptx-1,nptx-1),Temp(nptx-1,npty-1),&
ux_centres_faces(nptx-1,npty-1),uy_centres_faces(nptx-1,nptx-1),&
flux_adv_bas(nptx,npty),flux_adv_haut(nptx,npty),flux_adv_gauche(nptx,npty),flux_adv_droit(nptx,npty),&
flux_tot(nptx,npty),flux_adv_x(nptx,npty),flux_adv_y(nptx,npty))


CALL maillage()
CALL champ_vitesse()
CALL champ_temp()
CALL calcul_flux_advectif()
!CALL affichage_sortie()
CALL VTSWriter(0,0,nptx,npty,xnoeuds,ynoeuds,Temp,ux_centres_vol,uy_centres_vol,'ini')

npttemps=int(tfinal/dt) !TODO rectifier ça parce que c'est de la merde
PRINT*,npttemps
DO k=1,npttemps-1
  CALL calcul_flux_advectif()
!	CALL flux_diff
  CALL maj_temp()
  CALL VTSWriter(k*dt,k,nptx,npty,xnoeuds,ynoeuds,Temp,ux_centres_vol,uy_centres_vol,"int")
END DO

CALL VTSWriter(tfinal,npttemps,nptx,npty,xnoeuds,ynoeuds,Temp,ux_centres_vol,uy_centres_vol,"end")

DEALLOCATE(xnoeuds,ynoeuds,xcentre_vol,ycentre_vol,xcentre_faces_horiz,ycentre_faces_horiz,&
xcentre_faces_vertic,ycentre_faces_vertic,Temp,flux_adv_gauche,flux_adv_haut,flux_adv_droit,flux_adv_bas,&
ux_centres_vol,ux_centres_faces,uy_centres_faces,uy_centres_vol,flux_tot,flux_adv_y,flux_adv_x)


END PROGRAM main
