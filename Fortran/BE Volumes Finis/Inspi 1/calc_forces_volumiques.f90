subroutine calc_forces_volumiques(force_u,force_v,temp)

	USE Parametres
	USE Constantes

	implicit none
    

real*8 ,dimension(-2:nx+3,-2:ny+3),intent(out) :: force_u,force_v
real*8 ,dimension(-2:nx+3,-2:ny+3),intent(in) :: temp


real*8 ,dimension(-2:nx+3,-2:ny+3) :: rho_x,rho_y

integer :: i,j
	do i=1,nx
    do j=1,ny
      
       rho_x(i,j)=rho_ref*(1.0d0-alpha*((temp(i+1,j)+temp(i,j))*0.5-temp_ref))
       rho_y(i,j)=rho_ref*(1.0d0-alpha*((temp(i,j+1)+temp(i,j))*0.5-temp_ref))
       force_u(i,j)=(rho_x(i,j)/rho_ref)*grav_x
       force_v(i,j)=(rho_y(i,j)/rho_ref)*grav_y
       
     end do
     end do


end subroutine calc_forces_volumiques

