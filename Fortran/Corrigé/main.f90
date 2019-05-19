	program rayleigh_benard
!*******************************************************************************!
!	APPEL DES MODULES															!
!*******************************************************************************!

	USE Parametres
	USE Constantes
	implicit none 
 	
	
	
	
	
	
!    
!   COORDONNEES DES CENTRES DES CELLULES 
!
	real*8 ,dimension(1:nx) :: xx !coordonnee horizontale
	real*8 ,dimension(1:ny) :: yy !coordonnee verticale 
	
!    
	
!
!   INCONNUES DU PROBLEME
!    
    real*8 ,dimension(-2:nx+3,-2:ny+3) :: u,v              ! champ de vitesse en x et y
    real*8 ,dimension(-2:nx+3,-2:ny+3) :: temp             ! champ de temperature
    real*8 ,dimension(-2:nx+3,-2:ny+3) :: force_u,force_v  ! force volumique

!
!   AUTRES VARIABLES
!    
    real*8 ,dimension(1:nx,1:ny):: div,pres,rhs !divergence du champ de vitesse, pression, second membre de l'equation de pression
    real*8 ,dimension(-2:nx+3,-2:ny+3) :: lu,lv,lubis,lvbis ! tableaux de travail contenant les termes de convection pour u et v
                                                            ! pour l'intégration en temps par Adams Bashforth
    real*8 ,dimension(-2:nx+3,-2:ny+3) :: viscu,viscv       ! tableaux de travail contenant les termes visqueux pour u et v
 
    real*8 ,dimension(-2:nx+3,-2:ny+3) :: ltemp,ltempbis    ! tableaux de travail contenant les termes de convection pour la temperature
                                                            ! pour l'intégration en temps par Adams Bashforth
    real*8 ,dimension(-2:nx+3,-2:ny+3) :: visct             ! tableaux de travail contenant les termes de diffusion thermique 
                                                            ! pour l'equation de convection diffusion de la temperature
    real*8 ,dimension(1:nx,1:ny)       :: u_cent,v_cent     ! tableaux des composantes de la vitesse au centre
                                                            ! de la cellule -> pour les representations graphiques
    real*8 ,dimension(1:nx,1:ny)       :: vort              ! Vorticite du champ de vitesse au centre de la cellule de
                                                            ! calcul -> pour les representations graphiques

!
!   INDICES ENTIER
!    
	integer i,j,k,nstep_fin,istep1,l
	
!
!	AUTRES VARIABLES DE TRAVAIL
!
    real*8 :: sum,umax,vmax,max_comp,calc_dt,time0,Ra,Prandtl
    character(5) :: schema
    character(4) :: Rayleigh
    character(20) :: nom_fichier


	call system('rm *.scl *.geo *.case *.vec')
	

!	
!	CALCUL DES CENTRES DES CELLULES DE CALCUL (utilisé pour les sorties graphiques)
!
	do i=1,nx
	 xx(i)=xmin+(i-0.5)*dx
	enddo
	
	do j=1,ny
	 yy(j)=ymin+(j-0.5)*dy
	enddo
	

!   CHOIX DU SCHEMA D'ADVECTION


	write(*,*) 'Quel schema souhaitez vous utiliser pour l advection'
	write(*,*) '               amont ou  centre ?'
	
	read*,schema


!
!   CALCUL DE CERTAINS NOMBRES ADIMENSIONNELS 
!	

!
!   Nombre de Rayleigh
!	
	Ra=abs(grav_y)*abs(temp_haut-temp_bas)*alpha*(ymax-ymin)**3/nu/kappa
	write(*,*) 'Nombre de Rayleigh=', Ra
	
!
!   Nombre de Prandlt
!	
	Prandtl=nu/kappa
	write(*,*) 'Nombre de Prandlt=', Prandtl	
	

!	Ouverture fichier donnees


	write(Rayleigh,'(i4)')floor(Ra)
	
	nom_fichier='profils_'//trim(Rayleigh)//'.xy'

	Open(unit=20,file=nom_fichier,form='formatted')	
	 
!
!   INITIALISATION 
!

     u=0.
     v=0.
     force_u=0.0
     force_v=0.0
     temp=temp_ref

!
!   LECTURE D'UN FICHIER DE REPRISE SI LE FLAG REPRISE=1
!
        nstep_fin=0
        time0=0.
	if ( reprise == 1 ) then
	     call read_reprise(u,v,temp,lu,lv,ltemp,nstep_fin)
    endif
	time0=time0+t
!    
!    BOUCLE EN  TEMPS 
!        

      call bc_uv(u,v)
      call bc_temp(temp)

   do istep=1,nstep

!
!  CALCUL DU PAS DE TEMPS
!
      umax=0.
      vmax=0.
    
      umax=max_comp(u,nxu,nyu)
      vmax=max_comp(v,nxv,nyv)

      dt=cfl*calc_dt(umax,vmax)    
     
      
      write(*,*) 'istep=',istep, 'dt=',dt, 'time=',t

!
!    CALCUL DU CHAMP DE TEMPERATURE   
!
      call bc_temp(temp)

!
!    calcul des termes visqueux pour temp
!
     call calc_visc_temp(temp,visct)


!
!    calcul des termes de convection pour temp
!

     ltempbis=ltemp
     
     call conv_temp(temp,u,v,ltemp,schema)

   
!
!    AVANCEMENT EN TEMPS PAR METHODE D'ADAMS-BASHFORTH POUR temp 
!


		do j=1,ny
			do i=1,nx
				temp(i,j) = temp(i,j) + dt*(  demi*(3.d0*ltemp(i,j)-ltempbis(i,j)) + visct(i,j))*ddx*ddy
			end do
		end do


!
!    CALCUL DU CHAMP DE VITESSE INTERMEDIAIRE   
!

!
!    calcul des termes visqueux pour u et v
!
     call calc_visc(u,v,viscu,viscv)
     
!
!   calcul des forces volumiques
!
	call calc_forces_volumiques(force_u,force_v,temp)

     lubis=lu
     lvbis=lv

!
!    calcul des termes de convection pour u
!

     call conv_u(u,v,lu)
!
!    calcul des termes de convection pour v
!

     call conv_v(u,v,lv)


!
!    AVANCEMENT EN TEMPS PAR METHODE D'ADAMS-BASHFORTH POUR u ET v 
!


		do j=1,nyu
			do i=1,nxu

				u(i,j) = u(i,j) + dt*(  demi*(3.d0*lu(i,j)-lubis(i,j)) + &
					                       viscu(i,j) + force_u(i,j) )
			end do
		end do
                                          

        do j=1,nyv
			do i=1,nxv

				v(i,j) = v(i,j) + dt*(	demi*(3.d0*lv(i,j)-lvbis(i,j)) + &
					                        viscv(i,j) +   force_v(i,j) )

			end do
		end do

!
!        Calcul du second membre de l'equation de pression
!


	do j=1,ny
	   do i=1,nx
          div(i,j) = (u(i,j)-u(i-1,j))*ddx + (v(i,j)-v(i,j-1))*ddy
	      rhs(i,j) = -div(i,j)/dt
	   enddo
	enddo
      
!
!	GENERATION DE LA MATRICE des coef pour la resolution de l'equation de pression
!
	call matgen(coef,jcoef,nx,ny,ndim,mdim,dx,dy) 
	

	sum=0.0
	do j=1,ny
	   do i=1,nx
	      sum=sum+rhs(i,j)
	   enddo
	enddo

        sum=sum/float(nx*ny)
	k=1
	do j=1,ny
	   do i=1,nx
	      rhs(i,j)=rhs(i,j)-sum
	      rhs1(k)=rhs(i,j)
	      x1(k)=0.
	      k=k+1
	    enddo
	enddo
!	
!	RESOLUTION PRESSION
!
 	call ICCG2(coef,jcoef,l_s,Ldiag,rhs1,x1,ndim,mdim,zeta,p_s,r_s,r2_s,q_s,s_s,itmax)
	k=1
	do j=1,ny 
	   do i=1,nx 
  	      pres(i,j)=x1(k)
	      k=k+1
	    enddo
	enddo
	
!
!   CORRECTION DU CHAMP DE VITESSE
!

    do j=1,nyu
       do i=1,nxu
          
          u(i,j)=u(i,j)-dt*(pres(i+1,j)-pres(i,j))*ddx

       enddo
    enddo 

    do j=1,nyv
       do i=1,nxv
          
          v(i,j)=v(i,j)-dt*(pres(i,j+1)-pres(i,j))*ddy

       enddo
    enddo 


!
!   CONDITIONS AUX LIMITES SUR LA VITESSE
!    
	
    call bc_uv(u,v)
    
 
!
! 	ECRITURE DES RESULTATS
!
 	
    if(mod(istep,isto) == 0 )then 
 	
       do j=1,ny
	       do i=1,nx
              div(i,j) = (u(i,j)-u(i-1,j))*ddx + (v(i,j)-v(i,j-1))*ddy
	       enddo
	   enddo
       call calc_vort_uv_centre(u,v,vort,u_cent,v_cent)
       call write_result_ensight(xx,yy,u_cent,v_cent,vort,pres,temp,div,nx,ny,nz,istep,isto,nstep,nstep_fin,time0,dt)	

    endif
	
	
	! Ecriture des vitesses en fonction du temps en un point choisi
	


	
		do l=1,nx
		write(20,*)t,sqrt(((u(l-1,floor(0.5*ny))+u(l,floor(0.5*ny)))/2)**2+ &
						((v(l,floor((0.5*ny)-1)+v(l,floor(0.5*ny))))/2)**2), &
						temp(l,floor(0.5*ny))
		
		end do
	
	
    t=t+dt
    

    enddo
	
	
    Close(unit=20)
!
! 	ECRITURE DU FICHIER DE REPRISE
!
 	nstep_fin=nstep_fin+nstep
        write(*,*) 'nstep-fin=',nstep_fin
	call write_reprise(u,v,temp,lu,lv,ltemp,nstep_fin)
	
	
	write(*,*) 'Nombre de Rayleigh=', Ra

    call system('paraview sortieEnsight.case')

	end 
