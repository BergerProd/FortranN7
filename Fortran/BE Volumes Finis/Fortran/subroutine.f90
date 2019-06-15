!*******************
SUBROUTINE read_data
!*******************
!lecture du fichier d'entrée

USE module_reacteur_chimique
IMPLICIT NONE
LOGICAL     :: file_exists

INQUIRE(FILE='parametres.in',exist=file_exists)

IF (file_exists) THEN
    OPEN(1,FORM='FORMATTED',file='parametres.in')
    READ(1,*)
    READ(1,*)
    READ(1,*)cfl      !nombre de courant
    READ(1,*)fourier  !nombre de fourier
    READ(1,*)tfinal   !temps final
    READ(1,*)nptx     !nombre de points en x
    READ(1,*)npty     !nombre de points en y
    READ(1,*)
    READ(1,*)l        !longueur du domaine
    READ(1,*)a        !paramètre A amplitude
    READ(1,*)alpha_a
    READ(1,*)alpha_b
    READ(1,*)Ta
    READ(1,*)Tb
    READ(1,*)T0
    CLOSE(1)
ELSE
    PRINT*,' le fichier parametres.in n a pas été trouvé'
    STOP
END IF


END SUBROUTINE read_data

!******************
SUBROUTINE maillage
!******************
!Maillage de tout le domaine au noeuds,au centre, sur les deux types de faces verticales et horizontales
USE module_reacteur_chimique

IMPLICIT NONE
INTEGER         :: i, j

!Pas dx et dy
dx = L/real(nptx-1)
dy = L/real(npty-1)

!initialisation des noeuds
xnoeuds(1,:) = -L/2.d0
ynoeuds(:,1) = -L/2.d0

!Noeuds des mailles
DO i=2,npty
    xnoeuds(i,:) = xnoeuds(1,1)+dx*(i-1)
    ynoeuds(:,i) = ynoeuds(1,1)+dy*(i-1)
END DO

!Centre des Volumes
!Donc dx/2 en partant de chaque noeud en x et y
!npt-1 sur les 2
xcentre_vol(1,:) = -L/2.d0 + dx/2.d0
ycentre_vol(:,1) = -L/2.d0 + dx/2.d0

DO i=2,npty-1
    xcentre_vol(i,:) = xcentre_vol(1,1) + dx*(i-1)
    ycentre_vol(:,i) = ycentre_vol(1,1) + dx*(i-1)
END DO


!Faces
!On veut le centre des faces

!Faces horizontales
!egales aux noeuds en x + dx/2 i.e. abcisse des centre des faces ->npt-1
!meme coordonnées en y que les noeuds -> npt
xcentre_faces_horiz(:,:)=xcentre_vol(:,:)
ycentre_faces_horiz(:,:)=ynoeuds(:,:)


!Faces Verticales
!egales aux noeuds pour les abcisses -> npt
!egales ordonnées des centres ->npt-1
xcentre_faces_vertic(:,:)=xnoeuds(:,:)
ycentre_faces_vertic(:,:)=ycentre_vol(:,:)

END SUBROUTINE maillage

!***********************
SUBROUTINE champ_vitesse
!***********************
!Calcul du champ des vitesses aux centres des volumes

USE module_reacteur_chimique

IMPLICIT NONE
INTEGER     :: i, j

DO i=1,nptx-1
    DO j=1,npty-1
    ux_centres_vol(i,j) = a*DCOS(pi*((xcentre_vol(i,j)/L) -0.5d0))*DSIN(pi*((ycentre_vol(i,j)/L) -0.5d0)) !Ux
    uy_centres_vol(i,j) = -a*DSIN(pi*((xcentre_vol(i,j)/L) -0.5d0))*DCOS(pi*((ycentre_vol(i,j)/L) -0.5d0)) !Uy
    END DO
END DO

END SUBROUTINE champ_vitesse

!***************************
SUBROUTINE champ_temp
!***************************
!Initialisation de la température au centre du domaine à T0
!Calcul des profils de températures gaussien TfaceAC et TfaceBD avec le centre des faces verticales avec Ta et Tb
!T0, Ta,Tb sont rentré dans le fichier d'entrée en Kelvin.
USE module_reacteur_chimique

IMPLICIT NONE
INTEGER     :: j
REAL(KIND=8):: sigmaA,sigmaB

Temp(:,:) =T0

sigmaA=L/20.d0
sigmaB=L/20.d0

! Cas profil gaussien
DO j=1,npty-1
  TfaceAC(j)=(Ta-T0)*exp((-ycentre_faces_vertic(1,j)**2)/(2.d0*sigmaA**2))+T0 !Ta
  TfaceBD(j)=(Tb-T0)*exp((-ycentre_faces_vertic(nptx-1,j)**2)/(2.d0*sigmaB**2))+T0 !Tb
END DO

END SUBROUTINE champ_temp

!******************************
SUBROUTINE calcul_dt
!*****************************
!Calcul du pas de temps selon la formule donnée. CFL et fourier sont fournis dans le fichier d'entrée
!alpha_moy est la moyenne des alpha des gaz A et B
USE module_reacteur_chimique

IMPLICIT NONE


alpha_moy = (alpha_a + alpha_b)/2.d0
dt=1.d0/ (((ABS(MINVAL(ux_centres_vol)))/(cfl*dx)) + (ABS(MINVAL(uy_centres_vol))/(cfl*dx)) + (alpha_moy/fourier*(1/(dx*dx)+1/(dy*dy))))

END SUBROUTINE calcul_dt

!******************************
SUBROUTINE calcul_flux_advectif
!******************************
!Calcul des flux Advectifs selon la discrétisation du rapport
!Implantation des conidtions limites
USE module_reacteur_chimique

IMPLICIT NONE
INTEGER     :: i, j

DO i=2,nptx-1
  DO j=1,npty-1
    IF (ux_centres_vol(i,j)>=0)THEN
     flux_adv_x(i,j)=(Temp(i-1,j)*ux_centres_vol(i,j))*dy!(ycentre_faces_vertic(i,j+1) - ycentre_faces_vertic(i,j)

    ELSE
      flux_adv_x(i,j)=(Temp(i,j)*ux_centres_vol(i,j))*dy!(ycentre_faces_vertic(i,j+1)-ycentre_faces_vertic(i,j))
   END IF
  END DO
END DO

DO i=1,nptx-1
  DO j=2,npty-1
    IF (uy_centres_vol(i,j)>=0)THEN
      flux_adv_y(i,j)=(Temp(i,j-1)*uy_centres_vol(i,j))*dx!(xcentre_faces_horiz(i+1,j)-xcentre_faces_horiz(i,j))

    ELSE
      flux_adv_y(i,j)=(Temp(i,j)*uy_centres_vol(i,j))*dx!(xcentre_faces_horiz(i+1,j)-xcentre_faces_horiz(i,j))

    ENDIF
  END DO
END DO

!Conditions Limites
DO j=1,npty-1
  flux_adv_x(1,j) = ux_centres_vol(1,j)*TfaceAC(j)*dy
  flux_adv_x(nptx,j) = ux_centres_vol(nptx-1,j)*TfaceBD(j)*dy!Temp(nptx-1,j)*dy
END DO

DO i=1,nptx-1
  flux_adv_y(i,1) = uy_centres_vol(i,1)*Temp(i,1)*dx !soit Temp(i,1) soit Temp(i,2)
  flux_adv_y(i,npty) = uy_centres_vol(i,npty-1)*Temp(i,npty-1)*dx !soit Temp(i,npty-1) soit Temp(i,npty-2)
END DO

END SUBROUTINE calcul_flux_advectif

!***************************
SUBROUTINE calcul_flux_diff
!***************************
!Calcul des flux diffusifs, ne marche pas pour le moment
USE module_reacteur_chimique

INTEGER :: i,j

DO i=2,nptx-2
  DO j=1,npty-1
    flux_diff_x(i,j)=alpha_a*((Temp(i+1,j)-Temp(i,j))/dx - (Temp(i,j)-Temp(i-1,j))/(dx-1))*dy
  END DO
END DO

!DO i=2,nptx-1
!  DO j=1,npty-1
!    flux_diff_x(i,j)=alpha_a*((Temp(i,j)-Temp(i-1,j))/dx)
!  END DO
!END DO

DO i=1,nptx-1
  DO j=2,npty-2
    flux_diff_y(i,j)=alpha_b*((Temp(i,j+1)-Temp(i,j))/dy - (Temp(i,j)-Temp(i,j-1))/(dy-1))*dx
  END DO
END DO

!DO i=1,nptx-1
!  DO j=2,npty-1
!    flux_diff_y(i,j)=alpha_b*((Temp(i,j)-Temp(i,j-1))/dy)
!  END DO
!END DO

!Condition aux limites
!TODO il semblerait que les conditions limites ne marchent pas
!DO j=1,npty-1
!  flux_diff_x(1,j)        = alpha_a*((Temp(1,j)-TfaceAC(j))/(dx/2.d0))*dy
!  flux_diff_x(nptx,j)     = alpha_a*((TfaceBD(j)-Temp(nptx-1,j))/(dx/2.d0))*dy
!END DO

!TODO faire cette méthode plus les npt-1 en CL et Faire en maj temp juste flux diff (i) et de y

DO j=1,npty-1
  !flux_diff_x(1,j)      =alpha_a*((Temp(2,j)-Temp(1,j))/dx - (Temp(1,j)-TfaceAC(j))/dx)*dy
  flux_diff_x(1,j)      =alpha_a*((Temp(1,j)-TfaceAC(j))/dx - (TfaceAC(j)-TfaceAC(j))/dx)*dy !Test
  !flux_diff_x(2,j)      =alpha_a*((Temp(2,j)-Temp(1,j))/dx - (Temp(1,j)-TfaceAC(j))/dx)*dy !Test


  flux_diff_x(nptx-1,j)   =alpha_a*((TfaceBD(j)-Temp(nptx-1,j))/dx - (Temp(nptx-1,j)-Temp(nptx-2,j))/dx)*dy
  flux_diff_x(nptx,j)     =alpha_a*((TfaceBD(j)-TfaceBD(j))/dx - (TfaceBD(j)-Temp(nptx-1,j))/dx)*dy
END DO

DO i=1,nptx-1
  flux_diff_y(i,1) = flux_diff_y(i,2)
  flux_diff_y(i,npty-1)= flux_diff_y(i,npty-2)
  flux_diff_y(i,npty) = flux_diff_y(i,npty-1)
END DO


END SUBROUTINE calcul_flux_diff

!********************
SUBROUTINE maj_temp
!*******************
!Mise à jour de la température au temps t+1
USE module_reacteur_chimique

!Temperature est définie au centre des volumes de controle donc nx-1 * ny-1
DO i=1,nptx-1
  DO j=1,npty-1
    Temp(i,j)= Temp(i,j)+dt/(dx*dy)*(flux_adv_y(i,j)-flux_adv_y(i,j+1)+flux_adv_x(i,j)-flux_adv_x(i+1,j)+&
    !flux_diff_y(i,j)-flux_diff_y(i,j+1)+flux_diff_x(i,j)-flux_diff_x(i+1,j))
    flux_diff_x(i,j)+flux_diff_y(i,j))
  END DO
END DO

END SUBROUTINE maj_temp


!******************
subroutine VTSWriter(Time,Step,nx,ny,x,y,T,U,V,opt)
!-----------------------------------------------------------------------------#
!  Time    : Reel, temps physique                                             #
!  Step    : Entier, pas de temps = numero dans le nom de fichier,            #
!            si Step<0 on ecrit !                                             #
!            dans sol_exacte.vts, entier                                      #
!  nx      : Entier, nombre des noeuds en direction x                         #
!  ny      : Entier, nombre des noeuds en direction y                         #
!  x       : Tableau reel (de taille nx,ny) des abscisses des noeuds          #
!            des  volumes                                                     #
!  y       : Tableau reel (de taille nx,ny) des ordonnees des noeuds          #
!            des  volumes                                                     #
!  T,U,V   : Tableaux reel (de taille nx-1 par ny-1) des valeurs a tracer     #
!            (valeurs au centre des volumes de controle)                       #
!  opt     : Variable de type chaine des characteres qui doit prendre         #
!            l'une des valeurs suivantes :                                    #
!              - 'ini' pour le premier appel a VTSWriter                      #
!              - 'int' pour un appel standard a VTSWriter                     #
!              - 'end' pour le dernier appel a VTSWriter                      #
!-----------------------------------------------------------------------------#
  implicit none
  integer :: Step, i, j, nx, ny
  real(KIND=8) :: T(nx-1,ny-1),U(nx-1,ny-1),V(nx-1,ny-1),Time
  real(KIND=8) :: x(nx,ny), y(nx,ny)
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
  write(num2char,*) 3*nx*ny
  formatperso = '('//trim(num2char)//'(F15.6,1x))'
  write(8,'(a)') '<?xml version="1.0"?>'
  write(8,'(a)') '<VTKFile type="StructuredGrid">'
  write(8,'(a,6i6,a)') '<StructuredGrid WholeExtent="', 0,nx-1,0,ny-1,0,0,'">'
  write(8,'(a,6i6,a)') '<Piece Extent="',0,nx-1,0,ny-1,0,0,'">'
  write(8,'(a)') '<Points>'
  write(8,'(a)') '<DataArray type="Float32" NumberOfComponents="3"/>'
  ! - Ecriture des coordonnees -
  DO j=1,ny
     write(8,formatperso) (x(i,j),y(i,j),0.,i=1,nx)
  END DO
  write(8,'(a)') '</Points>'
  write(8,'(a)') '<CellData Scalars="Température, U, V">'
! T
  write(8,'(a)') '<DataArray type="Float32" Name="Temp, K"/>'
  write(num2char,*) (nx-1)*(ny-1)
  formatperso = '('//trim(num2char)//'(F15.6,1x))'
  ! - Ecriture des valeurs -
  DO j=1,ny-1
     write(8,formatperso) (T(i,j),i=1,nx-1)
  END DO
! U
  write(8,'(a)') '<DataArray type="Float32" Name="Vitesse u, m/s"/>'
  write(num2char,*) (nx-1)*(ny-1)
  formatperso = '('//trim(num2char)//'(F15.6,1x))'
  DO j=1,ny-1
     write(8,formatperso) (u(i,j),i=1,nx-1)
  END DO
! V
  write(8,'(a)') '<DataArray type="Float32" Name="Vitesse v, m/s"/>'
  write(num2char,*) (nx-1)*(ny-1)
  formatperso = '('//trim(num2char)//'(F15.6,1x))'
  DO j=1,ny-1
     write(8,formatperso) (v(i,j),i=1,nx-1)
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
