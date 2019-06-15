PROGRAM main
  !*********************************************
  !Bureau d'études Volumes finis
  !AUTELLET Adrien & BERGE Quentin
  !ENSEEIHT MF2E 1A
  !Juin 2019
  !*********************************************

USE module_reacteur_chimique
IMPLICIT NONE
INTEGER :: k

pi = 4.d0*DATAN(1.d0)

CALL read_data() !Lecture fichier d'entrée
!Allocation dynamique des tableaux
ALLOCATE(xnoeuds(nptx,npty),ynoeuds(nptx,npty))
ALLOCATE(xcentre_vol(nptx-1,npty-1),ycentre_vol(nptx-1,npty-1))
ALLOCATE(xcentre_faces_horiz(nptx,npty),ycentre_faces_horiz(nptx,npty))
ALLOCATE(xcentre_faces_vertic(nptx,npty),ycentre_faces_vertic(nptx,nptx))
ALLOCATE(ux_centres_vol(nptx-1,npty-1),uy_centres_vol(nptx-1,nptx-1),ux_centres_faces(nptx,npty-1),uy_centres_faces(nptx-1,npty))
ALLOCATE(Temp(nptx-1,npty-1),TfaceAC(npty-1),TfaceBD(npty-1))
ALLOCATE(flux_adv_x(nptx,npty-1),flux_adv_y(nptx-1,npty),flux_diff_x(nptx,npty-1),flux_diff_y(nptx-1,npty))


CALL maillage()
CALL champ_vitesse()
CALL champ_temp()!Initialisation des températures
CALL calcul_dt()
!dt = 1.d-6
WRITE(*,*) "Pas de Temps =", dt

CALL VTSWriter(0,0,nptx,npty,xnoeuds,ynoeuds,Temp,ux_centres_vol,uy_centres_vol,'ini')

npttemps = int(tfinal/dt +1)
WRITE(*,*)"nombre de points temps =",npttemps
DO k=1,npttemps-1!
  CALL calcul_flux_advectif()
  !CALL calcul_flux_diff()
  CALL maj_temp() !Mise à jour de la température à chaque instant
  CALL VTSWriter(k*dt,k,nptx,npty,xnoeuds,ynoeuds,Temp,ux_centres_vol,uy_centres_vol,"int")
END DO

CALL VTSWriter(tfinal,npttemps,nptx,npty,xnoeuds,ynoeuds,Temp,ux_centres_vol,uy_centres_vol,"end")

!Déallocation des tableaux dynamiques
DEALLOCATE(xnoeuds,ynoeuds,xcentre_vol,ycentre_vol,xcentre_faces_horiz,ycentre_faces_horiz,xcentre_faces_vertic,ycentre_faces_vertic)
DEALLOCATE(Temp,TfaceAC,TfaceBD)
DEALLOCATE(ux_centres_vol,ux_centres_faces,uy_centres_faces,uy_centres_vol)
DEALLOCATE(flux_adv_y,flux_adv_x,flux_diff_x,flux_diff_y)

END PROGRAM main
