SUBROUTINE conv_temp(temp,u,v,ltemp,schema)

	USE Parametres
	USE Constantes

	implicit none
	
	
	real*8 ,dimension(-2:nx+3,-2:ny+3),intent(in) :: u,v
	real*8 ,dimension(-2:nx+3,-2:ny+3),intent(inout)  :: temp
	real*8 ,dimension(-2:nx+3,-2:ny+3),intent(out) :: ltemp
	

	real*8, dimension(nx,ny) :: Fadv_o,Fadv_e,Fadv_n,Fadv_s
	character(5),intent(in) :: schema
	
	
	integer :: i,j
	
	
	if (schema=='centre') then
	
	
	 do i=1,nx
      do j=1,ny
      
       Fadv_e(i,j) = -1.0d0*u(i,j)*(temp(i+1,j)+temp(i,j))*0.5*dy
       Fadv_o(i,j) = u(i-1,j)*(temp(i,j)+temp(i-1,j))*0.5*dy
       
       Fadv_n(i,j) = -1.0d0*v(i,j)*(temp(i,j+1)+temp(i,j))*0.5*dx
       Fadv_s(i,j) = v(i,j-1)*(temp(i,j)+temp(i,j-1))*0.5*dx
       
      end do
     end do
	
	
	else
 
	 do i=1,nx
      do j=1,ny
      
      if (u(i,j)>=0) then
      
       Fadv_e(i,j) = -1.0d0*u(i,j)*(temp(i,j))*dy
       Fadv_o(i,j) = u(i-1,j)*(temp(i-1,j))*dy
       
       else 
       Fadv_e(i,j) = -1.0d0*u(i,j)*(temp(i+1,j))*dy
       Fadv_o(i,j) = u(i-1,j)*(temp(i,j))*dy
      
      end if
      
      if (v(i,j)>=0) then
      
      
       Fadv_n(i,j) = -1.0d0*v(i,j)*(temp(i,j))*dx
       Fadv_s(i,j) = v(i,j-1)*(temp(i,j-1))*dx
       
      else 
       Fadv_n(i,j) = -1.0d0*v(i,j)*(temp(i,j+1))*dx
       Fadv_s(i,j) = v(i,j-1)*(temp(i,j))*dx
       
       end if
      
      
      
      end do
     end do
     
     end if
  
  
  
  
  
     
     do i=1,nx
      do j=1,ny
    
    
      ltemp(i,j)=Fadv_o(i,j)+Fadv_e(i,j)+Fadv_n(i,j)+Fadv_s(i,j)
      
      end do
     end do
      
End subroutine conv_temp

