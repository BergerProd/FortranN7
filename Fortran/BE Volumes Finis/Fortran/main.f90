PROGRAM main

!TODO x et y en matriciel
!TODO centre, noeuds, faces

USE module_reacteur_chimique

pi = 4.d0*DATAN(1.d0)

CALL read_data()
ALLOCATE(x(nptx),y(npty),ux(nptx),uy(nptx))
CALL maillage()
CALL champ_vitesse()
CALL affichage_sortie()

DEALLOCATE(x,y,ux,uy)

END PROGRAM
