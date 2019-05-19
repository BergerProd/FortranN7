real*8 FUNCTION calc_dt(umax,vmax)

	USE Parametres
	USE Constantes

	implicit none

	real*8, intent(in) :: umax,vmax
	
	real*8 :: calc_dt_vit,calc_dt_temp

	real*8 :: conv_cfl,visc_cfl,grav_cfl,diff_cfl

	conv_cfl = dmax1(umax*ddx,vmax*ddy)
	visc_cfl = nu*(2.*ddx*ddx+2.*ddy*ddy)
        diff_cfl = kappa*(2.*ddx*ddx+2.*ddy*ddy)
	grav_cfl = dsqrt(dmax1(dabs(grav_x),dabs(grav_y))*ddy)

	calc_dt_vit = 2.0d0/(conv_cfl+visc_cfl+diff_cfl+grav_cfl)

    calc_dt_temp = 1.0d0/( dabs(umax)/(dx) + dabs(vmax)/(dy) + kappa*(1./(dx*dx)+1./(dy*dy))/(R*cfl) )

	calc_dt = min(calc_dt_vit,calc_dt_temp)

	return

END FUNCTION calc_dt

real*8 FUNCTION max_comp(vit,nx1,ny1)

	USE Parametres
	USE Constantes

	implicit none

	real*8, dimension(-2:nx+3,-2:ny+3), intent(in) :: vit

	integer :: i,j,nx1,ny1

	max_comp = 0.0
	
	do j=1,ny1
		do i=1,nx1
			max_comp = dmax1(max_comp,dabs(vit(i,j)))
		end do
	end do

	return

END FUNCTION max_comp
