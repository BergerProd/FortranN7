PROGRAM genere_sortie_aleat_prgm
IMPLICIT NONE

REAL(KIND=8),DIMENSION(4)     :: u
INTEGER                       :: i,npts=201
REAL(KIND=8)                  :: pi,pas


OPEN(1,file="sortie.dat")

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

close(1)


END PROGRAM genere_sortie_aleat
