PROGRAM calculfactoriel
IMPLICIT NONE

!declaration variables
INTEGER :: i,resentier
REAL(KIND=8) :: resreal,factoriel


!initialisation variables
resentier = 1
factoriel = 1.d0
resreal=1.d0
!demande factoriel
write(*,*)"quel factoriel voulez vous calculer ?"
read(*,*)factoriel
DO i=1,int(factoriel)
	resentier=i*resentier
	resreal=real(i)*resreal
END DO

!affichage
print*,"le resultat en entier",resentier
print*,"le resultat en reel",resreal


END PROGRAM calculfactoriel
