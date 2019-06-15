!*********************************************
!Bureau d'études Volumes finis
!LAPORTE Hugo & MASTIO Simon
!ENSEEIHT 1AHY
!Calcul scientifique et Programmation 2
!Mai 2018
!*********************************************

PROGRAM Volumes_finis

USE module_VF
IMPLICIT NONE

!Lecture du fichier de données
CALL lecture_fichier(L,A,alpha_a,alpha_b,Nx,Ny)

ALLOCATE(X(Nx),Y(Ny))
ALLOCATE(tab_x(Nx,Ny))
ALLOCATE(tab_y(Nx,Ny))
ALLOCATE(tab_xc(Nx-1,Ny-1))
ALLOCATE(tab_yc(Nx-1,Ny-1))
ALLOCATE(T(Nx-1,Ny-1),U(Nx-1,Ny-1),V(Nx-1,Ny-1),dt_loc(Nx-1,Ny-1))
ALLOCATE(T_A(Ny-1),T_B(Ny-1))
ALLOCATE(flux_adv_X(Nx,Ny-1))
ALLOCATE(flux_adv_Y(Nx-1,Ny))
ALLOCATE(flux_diff_X(Nx,Ny-1),flux_diff_Y(Nx-1,Ny))


!Allocation des tableaux

T(:,:)=To;
U(:,:)=0.
V(:,:)=0.
flux_adv_X(:,:)=0.
flux_adv_Y(:,:)=0.
flux_diff_X(:,:)=0.
flux_diff_Y(:,:)=0.



CALL maillage(x1,x2,dx,y1,y2,dy)
CALL vitesse
CALL temperature
CALL temps
CALL VTSWriter(0.,0,Nx,Ny,tab_x,tab_y,T,U,V,"ini")


Nt=int(Tf/dt)
Print*,dt
PRINT*,Nt

DO k=1,Nt
	!CALL flux_adv
	CALL flux_diff
	CALL maj_temp
	CALL VTSWriter(k*dt,k,Nx,Ny,tab_x,tab_y,T,U,V,"int")
END DO

CALL VTSWriter(Tf,Nt,Nx,Ny,tab_x,tab_y,T,U,V,"end")

END PROGRAM
