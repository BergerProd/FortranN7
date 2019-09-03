SUBROUTINE calc_visc_temp(temp,visct)

	USE Parametres
	USE Constantes
	
	implicit none
	

    real*8 ,dimension(-2:nx+3,-2:ny+3),intent(inout)  :: temp
    real*8 ,dimension(-2:nx+3,-2:ny+3),intent(out)  :: visct  
    
    real*8 ,dimension(nx,ny)  :: Fdiff_e,Fdiff_o,Fdiff_n,Fdiff_s
    
    
    
    integer:: i,j
!
!   CALCUL DES TERMES VISQUEUX POUR L'EQUATION DE TEMPERATURE
!

    do i=1,nx
       do j=1,ny
       Fdiff_e(i,j) = (temp(i+1,j)-temp(i,j))/dx*dy

       Fdiff_o(i,j) = -1.0d0*(temp(i,j)-temp(i-1,j))/dx*dy

       Fdiff_n(i,j) = (temp(i,j+1)-temp(i,j))/dy*dx

       Fdiff_s(i,j) = -1.0d0*(temp(i,j)-temp(i,j-1))/dy*dx
       end do
    end do                      	

	do i=1,nx
		do j=1,ny
		
		visct(i,j)=kappa*(Fdiff_e(i,j)+Fdiff_o(i,j)+Fdiff_n(i,j)+Fdiff_s(i,j))
		
		end do
	end do
	
	end subroutine calc_visc_temp
