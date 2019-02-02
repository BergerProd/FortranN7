PROGRAM calculpi
IMPLICIT NONE

!Declaration des Variables
INTEGER      :: i,nbriteration,j
REAL         :: pimet1simple,pimet2simple
REAL(KIND=8) :: pimet1double,pimet2double,piref,errmet1simple,errmet2simple
REAL (KIND=8) :: errmet2double,eps,errpimetbaileyfor,pimetbaileyfor,pimetbaileywhile,errpimetbaileywhile


!Traitement methode 1 simple precision

pimet1simple = 4.*ATAN(1.)

!Traitement methode 1 double precision

pimet1double = 4.d0*DATAN(1.d0)

!Traitement methode 2 simple precision

pimet2simple = 16.*ATAN(1./5.)-4.*ATAN(1./239.)

!Traitement methode 2 double precision

pimet2double = 16.d0*DATAN(1.d0/5.d0)-4.d0*DATAN(1.d0/239.d0)

!Traitement erreur
piref=pimet1double
errmet1simple=(pimet1simple/piref) -1.d0
errmet2simple=(pimet2simple/piref) -1.d0
errmet2double=(pimet2double/piref) -1.d0


!Traitement bailey

!methode FOR
!initialisation des Variables
i=0
pimetbaileyfor = 0

!demande nombre d'iteration
write(*,*)"écrire un nombre d'iteration"
read(*,*)nbriteration
!boucle de i=0 à nbriteration
DO i = 0, nbriteration
	pimetbaileyfor = pimetbaileyfor + ((1.d0/(16.d0**i))*((4.d0/((8.d0*i)+1.d0))&
	-(2.d0/((8.d0*i)+4.d0))-(1.d0/((8.d0*i)+5.d0))-(1.d0/((8.d0*i)+6.d0))))
END DO
!quand la ligne est trop longue il faut mettre & et sauter à l'autre ligne sinon erreur compilation
!traitement erreur pi met bailey for
errpimetbaileyfor = (pimetbaileyfor/piref) -1.d0


!methode WHILE
!initialisation des variables
j=0
errpimetbaileywhile = 1
pimetbaileywhile = 0
eps = 0
!demande erreur acceptable
write(*,*)"écrire l'erreur possible"
read(*,*)eps
!traitement while
DO WHILE (abs( errpimetbaileywhile) > abs(eps))
	! on passe en absolue sinon errpimetbaileywhile peut etre negative et sort de boucle
	pimetbaileywhile = pimetbaileywhile + ((1.d0/(16.d0**j))*&
	((4.d0/((8.d0*j)+1.d0)) -(2.d0/((8.d0*j)+4.d0))-&
	(1.d0/((8.d0*j)+5.d0))-(1.d0/((8.d0*j)+6.d0))))
	errpimetbaileywhile = (pimetbaileywhile/piref)-1.d0 !a chaque fois on recalcul l'erreur
	j=j+1 !incrementation indice
END DO




!Affichage
print*, "pi met1 en simple precision =", pimet1simple
print*,"pi met1 en double precision = ", pimet1double
print*,"pi met2 en simple precision =", pimet2simple
print*, "pi met2 en simple precision =", pimet2double
print*,"pi met bailey for",pimetbaileyfor
print*,"pi met bailey while",pimetbaileywhile


print*,"erreur methode 1 simple =", errmet1simple
print*,"erreur methode 2 simple =", errmet2simple
print*,"erreur methode 2 double =", errmet2double
print*,"erreur pi met pimetbailey for =", errpimetbaileyfor
print*,"erreur pi met pimetbailey while =", errpimetbaileywhile


END PROGRAM calculpi
