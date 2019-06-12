!**********************************************************************
!*************************BE VOLUMES FINIS*****************************
!************************DAYDE-THOMAS / LALU***************************
!**************************SUBROUTINES*********************************
!**********************************************************************

!*********************************************
SUBROUTINE read_data()
USE module_vfini
IMPLICIT NONE


LOGICAL     :: file_exists

! regarder l'utilisation de INQUIRE
INQUIRE(FILE='vfini.in',exist=file_exists)

!Lecture des différentes valeurs présentes dans le fichier balistiique.in
IF (file_exists) THEN
    OPEN(1,FORM='FORMATTED',file='vfini.in')
    READ(1,*)
    READ(1,*)L
    READ(1,*)alpha
    READ(1,*)A
    READ(1,*)nptx
    READ(1,*)npty
    READ(1,*)tfinal

    CLOSE(1)
ELSE
    PRINT*," le fichier vfini.in n'a pas été trouvé"
    STOP
END IF

END SUBROUTINE read_data

!*********************************************
SUBROUTINE maillage()
USE module_vfini
IMPLICIT NONE
INTEGER        :: i,j

deltax = L/(float(nptx)-1)
deltay = L/(float(npty)-1)
DO i=1,nptx
    x(1,i)=-L/2
    y(i,1)=-L/2
    xm(1,i)=-L/2+deltax/2
    ym(i,1)=-L/2+deltay/2
END DO
DO i=1,nptx
    DO j=1,npty
        x(i,j)=x(1,j)+deltax*(i-1)
        y(i,j)=y(i,1)+deltay*(j-1)
        PRINT*,"x",x(i,j)
        PRINT*,"y",y(i,j)
    END DO
END DO
DO i=1,nptx-1
    DO j=1,npty-1
        xm(i,j)=xm(1,j)+deltax*(i-1)
        ym(i,j)=ym(i,1)+deltay*(j-1)
        PRINT*,"xm",xm(i,j)
        PRINT*,"ym",ym(i,j)
    END DO
END DO

END SUBROUTINE maillage

!*********************************************
SUBROUTINE calcul_vitesse()
USE module_vfini
IMPLICIT NONE
INTEGER                                             :: i,j


DO i=1,nptx-1
    DO j=1,npty-1
        !T(i,j)=293.                          !T0 imposé dans tout le domaine
        !U(i,j)=A*cos(pi*((xm(i,j)/L)-0.5))*sin(pi*((ym(i,j)/L)-0.5))
        !V(i,j)=-A*sin(pi*((xm(i,j)/L)-0.5))*cos(pi*((ym(i,j)/L)-0.5))
        U(i,j)=U0
        V(i,j)=0*V0  
    END DO
END DO

END SUBROUTINE calcul_vitesse

!*********************************************
SUBROUTINE profil_temperature()
USE module_vfini
IMPLICIT NONE
INTEGER                                             :: i,j
REAL(KIND=4)                                        :: T1=493.,T0=293.,T2=600.




DO j=1,npty-1
        T(1,j)=(T1-T0)*exp((-ym(1,j)**2)/(2.*(L/20.)**2))+T0
        T(nptx-1,j)=(T2-T0)*exp((-ym(nptx-1,j)**2)/(2.*(L/20.)**2))+T0
END DO

DO i=1,nptx-1
    DO j=1,npty-1
        T(i,j)=T(i,j)+(Fd(i,j)+Fg(i,j)+Fb(i,j)+Fh(i,j)) *(dt/(deltax*deltay))
  
    END DO
END DO

END SUBROUTINE profil_temperature

!********************************************* test test test test test test test
SUBROUTINE calcul_flux1()
USE module_vfini
IMPLICIT NONE
INTEGER                        :: i,j
DO i=2,nptx-1
    DO j=1,npty-1
    Fd(i,j)=-U(i,j)*T(i,j)*deltay
    Fg(i,j)=U(i-1,j)*T(i-1,j)*deltay
    Fh(i,j)=-V(i,j)*T(i,j)*deltax
    Fb(i,j)=V(i,j-1)*T(i,j-1)*deltax
    END DO
END DO
END SUBROUTINE calcul_flux1

!*******************************************
SUBROUTINE maillage_temporel()
USE module_vfini
IMPLICIT NONE


!Pas de temps
REAL(Kind=4)                                        :: CFL=0.7

        dt = CFL*deltax/U0

END SUBROUTINE maillage_temporel


!********************************************************
SUBROUTINE affichage_sortie
!********************************************************

USE module_vfini
IMPLICIT NONE
CHARACTER(LEN=30)           :: nom
INTEGER                     :: i,j


nom='balistique_analytique_cas_'//'.out'

OPEN(1,FORM='FORMATTED',FILE=TRIM(nom))
WRITE(1,100)
100 FORMAT('#',3x,'x',8x,'y',8x,'U',8x,'V',8x) !!!,'Vx',8x,'Vz',8x)

DO i=1,nptx
    DO j=1,npty
    WRITE(1,200)i,j,x(i,j),y(i,j),U(i,j),V(i,j)  !!!! ,Vx(i),Vz(i)
    END DO
END DO

200 FORMAT(i4,2x,7(e13.6,3x))
CLOSE(1)

END SUBROUTINE affichage_sortie


!****************** 
subroutine VTSWriter(Time,Step,nptx,npty,x,y,T,U,V,opt)
!-----------------------------------------------------------------------------#
!  Time    : Reel, temps physique                                             #
!  Step    : Entier, pas de temps = numero dans le nom de fichier,            #
!            si Step<0 on ecrit !                                             #
!            dans sol_exacte.vts, entier                                      #
!  nptx      : Entier, nombre des noeuds en direction x                       #
!  npty      : Entier, nombre des noeuds en direction y                       #
!  x       : Tableau reel (de taille nptx,npty) des abscisses des noeuds      #
!            des  volumes                                                     #
!  y       : Tableau reel (de taille nptx,npty) des ordonnees des noeuds      #
!            des  volumes                                                     #
!  T,U,V   : Tableaux reel (de taille nptx-1 par npty-1) des valeurs a tracer #
!            (valeurs au centre des volumes de controle                       #
!  opt     : Variable de type chaine des characteres qui doit prendre         #
!            l'une des valeurs suivantes :                                    #
!              - 'ini' pour le premier appel a VTSWriter                      #
!              - 'int' pour un appel standard a VTSWriter                     #
!              - 'end' pour le dernier appel a VTSWriter                      #
!-----------------------------------------------------------------------------#
  implicit none
  integer :: Step, i, j, nptx, npty
  real :: T(nptx-1,npty-1),U(nptx-1,npty-1),V(nptx-1,npty-1),Time
  real :: x(nptx,npty), y(nptx,npty)
  character(100) :: num2char
  character(200) :: FileName, formatperso
  character(3) :: opt


  !  --- Ecriture d un fichier temporel au format paraview  ---
  if (Step >= 0) then
    write(num2char,'(i6.6)') Step
    FileName = 'sol_'//trim(num2char)//'.vts'
    open(8,file=FileName)
  else
    open(8,file='sol_exacte.vts',status='old')
  end if
  write(num2char,*) 3*nptx*npty
  formatperso = '('//trim(num2char)//'(F15.6,1x))'
  write(8,'(a)') '<?xml version="1.0"?>'
  write(8,'(a)') '<VTKFile type="StructuredGrid">'
  write(8,'(a,6i6,a)') '<StructuredGrid WholeExtent="', 0,nptx-1,0,npty-1,0,0,'">'
  write(8,'(a,6i6,a)') '<Piece Extent="',0,nptx-1,0,npty-1,0,0,'">'
  write(8,'(a)') '<Points>'
  write(8,'(a)') '<DataArray type="Float32" NumberOfComponents="3"/>'
  ! - Ecriture des coordonnees -
  DO j=1,npty
     write(8,formatperso) (x(i,j),y(i,j),0.,i=1,nptx)
  END DO
  write(8,'(a)') '</Points>'
  write(8,'(a)') '<CellData Scalars="Température, U, V">'
! T
  write(8,'(a)') '<DataArray type="Float32" Name="Temp, K"/>'
  write(num2char,*) (nptx-1)*(npty-1)
  formatperso = '('//trim(num2char)//'(F15.6,1x))'
  ! - Ecriture des valeurs -
  DO j=1,npty-1
     write(8,formatperso) (T(i,j),i=1,nptx-1)
  END DO
! U
  write(8,'(a)') '<DataArray type="Float32" Name="u"/>'
  write(num2char,*) (nptx-1)*(npty-1)
  formatperso = '('//trim(num2char)//'(F15.6,1x))'
  DO j=1,npty-1
     write(8,formatperso) (u(i,j),i=1,nptx-1)
  END DO
! V
  write(8,'(a)') '<DataArray type="Float32" Name="v"/>'
  write(num2char,*) (nptx-1)*(npty-1)
  formatperso = '('//trim(num2char)//'(F15.6,1x))'
  DO j=1,npty-1
     write(8,formatperso) (v(i,j),i=1,nptx-1)
  END DO
  write(8,'(a)') '</CellData>'
  write(8,'(a)') '</Piece>'
  write(8,'(a)') '</StructuredGrid>'
  write(8,'(a)') '</VTKFile>'
  close(8)


  ! - Remplissage du fichier "Collection" determinant l evolution temporelle -
  if (opt == 'ini' ) then
    open(10,file='sol.pvd')
    write(10,'(a)') '<?xml version="1.0"?>'
write(10,*) '<VTKFile type="Collection" version="0.1" format="ascii">'
    write(10,*) '<Collection>'
  else
    open(10,file='sol.pvd',position='append')
  end if
  if (Step >= 0) write(10,*) '<DataSet timestep="',Time,'" group="" part="0" file="',trim(FileName),'"/>'
  if ( opt == 'end') then
    write(10,*) '</Collection>'
    write(10,*) '</VTKFile>'
  end if
  close(10)

end subroutine VTSWriter
