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
  real :: T(nx-1,ny-1),U(nx-1,ny-1),V(nx-1,ny-1),Time
  real :: x(nx,ny), y(nx,ny)
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
