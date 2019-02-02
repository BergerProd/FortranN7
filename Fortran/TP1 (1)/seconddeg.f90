PROGRAM seconddeg
IMPLICIT none
!résolution equation second degré ax^2+bx+c = 0 en reel et en complexe
!BERGE Quentin

!declaration des variables
REAL(KIND=8) :: a,b,c,delta,x1,x2,x
COMPLEX      :: z1,z2
LOGICAL      :: deltapositif

!demande de saisie des valeurs
write(*,*)"saisir a ?"
read(*,*)a
write(*,*)"saisir b ?"
read(*,*)b
write(*,*)"saisir c ?"
read(*,*)c

!calcul du discriminant
delta = b**2 - 4*a*c
!calculs des solutions
IF ( delta < 0 ) then !à modifier pour avoir une valeur approchante de 0 par - ou +
  z1=CMPLX(-b, sqrt(-delta))/(2*a)
  z2=CONJG(z1) !z2, conjugué de z1
  deltapositif=.false.
ELSE IF (delta > 0) then
  x1 = (-b -SQRT(delta))/(2*a)
  x2 = (-b+SQRT(delta))/(2*a)
  deltapositif=.true.
ELSE !(delta<0) solutions complexes
  x1=-b/(2*a)
  x2=x1
  deltapositif=.true.
end if

!affichage des solutions
IF ( deltapositif ) then
  print*,"discriminant positif, x1 et x2 :"
  print*,x1,x2
ELSE
  print*,"discriminant negatif, z1 et z2 :"
  print*,z1,z2
END IF

END PROGRAM seconddeg
