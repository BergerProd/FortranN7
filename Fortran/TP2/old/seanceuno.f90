PROGRAM seanceuno
IMPLICIT none

INTEGER :: choix,m,n,npts

WRITE(*,*)"selectionner une option entre 1 et 4"
READ(*,*)choix

SELECT CASE (choix)
  	CASE(1)
      CALL genentree(m,n)

    CASE(2)
      CALL gensortie(npts)

    CASE(3)




    CASE(4)


    CASE DEFAULT
    	WRITE(*,*) "choix doit etre compris entre 1 et 4"
END SELECT
END PROGRAM seanceuno

!******************************
SUBROUTINE genentree(m,n)
!******************************
IMPLICIT none

INTEGER i,j,m,n
REAL, DIMENSION(:,:),Allocatable :: matentreealeat

!demande dimension n lignes, m colonnes
WRITE(*,*)"Rentrer m lignes"
READ(*,*)m
WRITE(*,*)"Rentrer n colonnes"
READ(*,*)n

ALLOCATE (matentreealeat(m,n))

CALL random_number(matentreealeat(:,:))

OPEN(1,file="entree.txt")

Do i = 1, m
  WRITE(1,*) matentreealeat(i,:)
End do

DEALLOCATE(matentreealeat)
close(1)

END SUBROUTINE genentree


!******************************
SUBROUTINE gensortie(npts)
!******************************
!genere un dossier de sortie .dat une matrice avec x,cos(x),sin(x),exp(x)
  IMPLICIT none
  INTEGER      :: o,npts
  REAL, DIMENSION(:,:), Allocatable :: matsortiex
  REAL(KIND=8) :: pi,pas


!ouverture fichier
  OPEN(2,file="sortie.dat")

!demande nombre de points
WRITE(*,*)"Rentrer le nombre de points"
READ(*,*)npts
!allocation dynamique pour le tableau en vertical, sinon 4 colonnes
ALLOCATE (matsortiex(npts,4))

!calcul de pi en double précision
pi = (4.d0*datan(1.d0))
!on definit le 1er et le dernier nombre en 1ere ligne et en derniere 1 et 4pi
matsortiex(0,1)=0
matsortiex(npts,1)=4*pi
!calcul le pas
pas=(matsortiex(npts,1)-matsortiex(0,1))/(float(npts-1))

!calul de la matrice avec tous les coeffcients
!parcours jusqu'au max npts
DO o=1,npts
  matsortiex(o,1)=pas*real(o-1)
  matsortiex(o,2)=cos(matsortiex(o,1))
  matsortiex(o,3)=sin(matsortiex(o,1))
  matsortiex(o,4)=exp(matsortiex(o,1))
END DO

!ecriture de la matrice dans
Do o = 1, npts
  WRITE(2,*) matsortiex(o,:)
End do

CLOSE(2)
DEALLOCATE(matsortiex)
END SUBROUTINE gensortie
