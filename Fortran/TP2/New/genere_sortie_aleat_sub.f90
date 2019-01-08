!*******************************************
SUBROUTINE genere_sortie_aleat_sub(npts,nom)
!*******************************************
!Subroutine générant un fichier de 4 colonnes avec de 0 a 4pi, x, cos(x),sin(x),e(x)

  IMPLICIT NONE

  REAL(KIND=16),DIMENSION(4)     :: u !vecteur pour les 4 colonnes
  REAL(KIND=16)                  :: pi,pas
  INTEGER                       :: i
  INTEGER,INTENT(IN)            :: npts
  CHARACTER(LEN=*),INTENT(IN)   :: nom


  OPEN(1,file=nom)

  !calcul de pi en double précision
  pi = 4.d0*datan(1.d0)
  !Calcul du pas
  pas = ((4.d0*pi)-0)/(npts-1.d0)


  do i=1,npts-1 !pour chaque ligne jusqu'a final point
    u(1) = float(i) * pas
    u(2) = dcos(u(1))
    u(3) = dsin(u(1))
    u(4) = dexp(u(1))
    WRITE(1,*),u
  END do

  CLOSE(1)


end SUBROUTINE genere_sortie_aleat_sub
