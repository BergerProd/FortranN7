PROGRAM seconddegcplx
IMPLICIT NONE
!Résolution equation second degré ax^2+bx+c = 0 en reel et en complexe
!se limite à une simple precision à cause de la fonction csqrt (racine carrée en complexe)
!BERGE Quentin

!declaration des Variables
COMPLEX  :: a,b,c,z1,z2,delta


!demande de saisie des valeurs
WRITE(*,*)"Saisir a sous la forme (partie reelle, imaginaire)?"
READ(*,*)a
WRITE(*,*)"saisir b sous la forme (partie reelle, imaginaire)?"
READ(*,*)b
WRITE(*,*)"saisir c sous la forme (partie reelle, imaginaire)?"
READ(*,*)c

!calcul du discriminant
delta = b**2.d0 - 4.d0*a*c

!traitement calcul avec fonction sqrt en complexe
z1=(-b-csqrt(delta))/(2.d0*a)
z2=(-b+csqrt(delta))/(2.d0*a)

!affichage des solutions et distinction pour racines doubles
IF ( z1==z2 ) THEN
  PRINT*,"La solution de l'équation est une racine double sous la forme (partie reelle, imaginaire)"
  PRINT*,z1
ELSE
  PRINT*,"Les solutions de l'équation sont deux racines simples sous la forme (partie reelle, imaginaire)"
  PRINT*,z1,z2
END IF




END PROGRAM seconddegcplx
