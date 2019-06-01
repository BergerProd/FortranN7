!***********************
!CREATION DU MAILLAGE
!***********************

SUBROUTINE maillage
USE module_VF

!Création des vecteurs
x1=-L/2.;
x2=L/2;
dx=(x2-x1)/REAL(Nx-1)
X(:)=0;

DO i=1,Nx
	X(i)= dx*REAL(i-1)+x1
END DO

y1=-L/2.;
y2=L/2.;
dy=(y2-y1)/REAL(Ny-1)
Y(:)=0;

DO i=1,Ny
	Y(i)= dy*REAL(i-1)+y1
END DO


!Création des tableaux
tab_x(:,:)=0;
tab_y(:,:)=0;

DO i=1,Nx
	DO j=1,Ny
		tab_x(i,j)=X(i)
		tab_y(i,j)=Y(j)
	END DO
END DO


DO i=1,Nx-1
	DO j=1,Ny-1
		tab_xc(i,j)=tab_x(i,j)+dx/2.
		tab_yc(i,j)=tab_y(i,j)+dy/2.
	END DO
END DO


END SUBROUTINE maillage


!******************************
!CALCUL DU CHAMP DE VITESSE
!******************************

SUBROUTINE vitesse
USE module_VF


!Calcul champ de vitesse

DO i=1,Nx-1
	DO j=1,Ny-1
		U(i,j)=1. !A*cos(3.14159*(tab_xc(i,j)/L-1/2))*sin(3.14159*(tab_yc(i,j)/L-1/2))
		V(i,j)=0. !-A*sin(3.14159*(tab_xc(i,j)/L-1/2))*cos(3.14159*(tab_yc(i,j)/L-1/2))
	END DO
END DO



END SUBROUTINE vitesse


!****************************
!CALCUL DES TEMPERATURES
!****************************

SUBROUTINE temperature
USE module_VF

sigmaA=L/20
sigmaB=L/20



DO j=1,Ny-1
	T_A(j)=(Ta-To)*exp((-tab_yc(1,j)**2)/(2*sigmaA**2))+To
	T_B(j)=(Tb-To)*exp((-tab_yc(Nx-1,j)**2)/(2*sigmaA**2))+To
END DO


END SUBROUTINE temperature


!****************************
!CALCUL DU PAS DE TEMPS
!****************************

SUBROUTINE temps
USE module_VF

 
DO i=1,Nx-1
	DO j=1,Ny-1
		dt_loc(i,j)= 1/((abs(U(i,j))/(dx*CFL)+abs(V(i,j))/&
		(dy*CFL)+(alpha_a/r)*(1/(dx*dx)+1/(dy*dy))))
	END DO
END DO

dt = minval(dt_loc);


END SUBROUTINE temps


!**************************
!CALCUL DES FLUX ADVECTIFS
!**************************

SUBROUTINE flux_adv
USE module_VF
INTEGER shift	
!Flux advectif sur x
DO i=2,Nx-1
	DO j=1,Ny-1
		shift = int(-0.5*(1+sign(1., 0.5*(U(i-1,j)+U(i,j))   ) ))
		flux_adv_X(i,j) = U(i+shift,j)*T(i+shift,j)*dy

	END DO
END DO

!Flux advectif sur y
DO i=1,Nx-1
	DO j=2,Ny-1
		shift = int(-0.5*(1+sign(1., 0.5*(V(i,j-1)+V(i,j))   ) ))
		flux_adv_Y(i,j) =V(i,j+shift)*T(i,j+shift)*dx

	END DO
END DO

!Conditions aux limites

DO j=1,Ny-1
!	flux_adv_X(1,j) = U(1,j)*400.*dy	exemple avec une température d'entrée uniforme
	flux_adv_X(1,j) = U(1,j)*T_A(j)*dy
	flux_adv_X(Nx,j) = U(Nx-1,j)*T(Nx-1,j)*dy
END DO

DO i=1,Nx-1
	flux_adv_Y(i,1) = V(i,1)*T(i,1)*dx
	flux_adv_Y(i,Ny) = V(i,Ny-1)*T(i,Ny-1)*dx

END DO


END SUBROUTINE flux_adv




!***************************
!CALCUL DES FLUX DIFFUSIFS
!***************************
SUBROUTINE flux_diff
USE module_VF

DO i=2,Nx-1
	DO j=1,Ny-1
		flux_diff_X(i,j)=-alpha_a*(T(i,j)-T(i-1,j))/dx*dy
	END DO
END DO

DO i=1,Nx-1
	DO j=2,Ny-1
		flux_diff_Y(i,j)=-alpha_a*(T(i,j)-T(i,j-1))/dy*dx
	END DO
END DO

!Condition aux limites
DO i=1,Nx-1
	flux_diff_Y(i,1) = flux_diff_Y(i,2)
	flux_diff_Y(i,Ny) = flux_diff_Y(i,Ny-1)
END DO


DO j=1,Ny-1
	flux_diff_X(1,j) = -alpha_a*(T_A(j)-T(1,j))/(dx/2.)*dy
	flux_diff_X(Nx,j) = -alpha_a*(T(Nx-1,j)-T_B(j))/(dx/2.)*dy
END DO




END SUBROUTINE flux_diff



!******************************
!CALCUL DE LA MAJ DE LA TEMP
!******************************
SUBROUTINE maj_temp
USE module_VF

DO i=1,Nx-1
	DO j=1,Ny-1
		T(i,j)=T(i,j)+dt/(dx*dy)*(flux_adv_Y(i,j)&
		-flux_adv_Y(i,j+1)+flux_adv_X(i,j)-flux_adv_X(i+1,j)&
!		+flux_diff_Y(i,j)-flux_diff_Y(i,j+1)+flux_diff_X(i,j)&
!		-flux_diff_X(i+1,j)&
)

	END DO
END DO




END SUBROUTINE maj_temp

