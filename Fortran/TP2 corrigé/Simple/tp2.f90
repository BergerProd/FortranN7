PROGRAM tp2
! Novembre 2013
! C. Airiau
! cas simple, passage des paramètres comme argument des subroutines
!************************************
! DECLARATION DES VARIABLES
!************************************
IMPLICIT NONE
INTEGER             :: option
INTEGER,PARAMETER   :: n=5, m=10, np=201
REAL(KIND=8),DIMENSION(n,m) :: mat

CALL menu(option)       

SELECT CASE (option)
    case(1)
        CALL genere_fichier_aleatoire(m,n,'entree.txt')

    case(2)
        CALL genere_fichier_fonction(np,'data.dat')

    case(3)
        CALL lire_fichier(m,n,'entree.txt',mat)
        PRINT*, 'Matrice lu : '
        PRINT*,mat
        PRINT*
        PRINT*
        ! voir la difference avec 
        PRINT'(5e15.5)',mat

    case(4)

        CALL genere_fichier_binaire(m,n,'entree.txt')

END SELECT



END PROGRAM tp2

!*******************
SUBROUTINE menu(opt)
!*******************

IMPLICIT NONE
INTEGER,INTENT(OUT)    :: opt

PRINT'(/,a,/)',' TP2 '
PRINT*,' Options possibles :'
PRINT*,'     < 1 > : question 1'
PRINT*,'     < 2 > : question 2'
PRINT*,'     < 3 > : question 3'
PRINT*,'     < 4 > : question 4'
PRINT*, 'Entrer votre choix :'
READ*, opt

IF ((opt.lt.0).or.(opt.gt.4)) THEN
      PRINT*, 'mauvais choix : stop'
      STOP
END IF


END SUBROUTINE menu


!*******************************************
SUBROUTINE genere_fichier_aleatoire(m,n,nom)
!*******************************************

IMPLICIT NONE
INTEGER,INTENT(IN)          :: n,m
CHARACTER(LEN=*)            :: nom
REAL(kIND=8),DIMENSION(n)   :: x
INTEGER                     :: i,j

OPEN(1,FORM='FORMATTED',FILE=nom)
DO i=1,m
    DO j=1,n
        x(j)=RAN()
    END DO
    WRITE(1,*)x
END DO

CLOSE(1)

END SUBROUTINE genere_fichier_aleatoire


!****************************************
SUBROUTINE  genere_fichier_fonction(n,nom)
!****************************************

IMPLICIT NONE
INTEGER,INTENT(IN)          :: n
CHARACTER(LEN=*)            :: nom
REAL(kIND=8)                :: x,dx
INTEGER                     :: i

dx=16.d0*DATAN(1.d0)/DFLOAT(n-1)
x=0.d0
OPEN(1,FORM='FORMATTED',FILE=nom)
DO i=1,n
    WRITE(1,'(4(e12.5,3x))')x,COS(x),SIN(x),EXP(x)
    x=x+dx
END DO
CLOSE(1)

END SUBROUTINE genere_fichier_fonction


!*******************************************
SUBROUTINE lire_fichier(m,n,nom,x)
!*******************************************

IMPLICIT NONE
INTEGER,INTENT(IN)          :: n,m
CHARACTER(LEN=*)            :: nom
REAL(kIND=8),DIMENSION(m,n),INTENT(OUT) :: x
INTEGER                     :: i


OPEN(1,FORM='FORMATTED',FILE=nom)
DO i=1,m
    READ(1,*)x(i,:)
END DO
CLOSE(1)


END SUBROUTINE lire_fichier

!*******************************************
SUBROUTINE genere_fichier_binaire(m,n,nom)
!*******************************************

IMPLICIT NONE
INTEGER,INTENT(IN)          :: n,m
CHARACTER(LEN=*)            :: nom
REAL(kIND=8),DIMENSION(m,n) :: x
REAL(kIND=8),DIMENSION(m,2) :: y
INTEGER                     :: i


OPEN(1,FORM='FORMATTED',FILE=nom)
DO i=1,m
    READ(1,*)x(i,:)
END DO
CLOSE(1)

OPEN(1,FORM='UNFORMATTED',FILE='sortie.bin')
DO i=1,m
    WRITE(1) x(i,1),SUM(x(i,2:n))
END DO
CLOSE(1)

! autre possibilité, voir les différences
y(:,1)=x(:,1)
DO i=1,m
    y(i,2)=SUM(x(i,2:n))
END DO
OPEN(1,FORM='UNFORMATTED',FILE='sortie1.bin')
WRITE(1) y
CLOSE(1)

END SUBROUTINE genere_fichier_binaire


