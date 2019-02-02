PROGRAM fibonacci
IMPLICIT none

!Calcul de la suite de fibonacci en utilisant un tableau et en rentrant n


INTEGER :: i,n
REAL,DIMENSION(10000) ::u

u(1)=1
u(2)=1

WRITE(*,*)"Quel U(n) calculer ?"
READ(*,*)n
DO i=3,n
  u(i)=u(i-1)+u(i-2)
END DO

PRINT*,"n=",n,":",u(i-2),"+",u(i-3),"=",u(n)


END PROGRAM fibonacci
