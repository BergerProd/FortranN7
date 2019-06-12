!**********************************************************************
!*************************BE VOLUMES FINIS*****************************
!************************DAYDE-THOMAS / LALU***************************
!************************PROGRAMME PRINCIPAL***************************
!**********************************************************************

Program volume_fini

USE module_vfini
IMPLICIT NONE

INTEGER                                             :: m
CALL read_data()
ALLOCATE (U(nptx-1,npty-1),V(nptx-1,npty-1),x(nptx,npty),y(nptx,npty),xm(nptx-1,npty-1),ym(nptx-1,npty-1),T(nptx-1,npty-1),Fd(nptx-1,npty-1),Fg(nptx-1,npty-1),Fh(nptx-1,npty-1),Fb(nptx-1,npty-1),Ftot(nptx-1,npty-1))
!Ta(1,npty),Tb(1,npty))
!ALLOCATE (dt(nptx-1,npty-1))

CALL maillage()
CALL calcul_vitesse()



CALL profil_temperature()
CALL calcul_flux1()
call maillage_temporel()

CALL VTSWriter(0.0,0,nptx,npty,x,y,T,U,V,'ini')

intervalles =int(tfinal/dt)
Print*,'intervalles',intervalles

DO m=1,intervalles
   CALL calcul_flux1()
   CALL profil_temperature()
   CALL VTSWriter(m*dt,m,nptx,npty,x,y,T,U,V,'int')
   PRINT*,"EN COURS",m
END DO
CALL VTSWriter(tfinal,intervalles,nptx,npty,x,y,T,U,V,'end')
DEALLOCATE (x,y,U,V,xm,ym,T,Fd,Fg,Fh,Fb,Ftot,)
!,dt,Ta,Tb
print *,"FIN DU PROGRAMME"
END PROGRAM volume_fini
