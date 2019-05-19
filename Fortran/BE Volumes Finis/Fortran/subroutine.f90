!*******************
SUBROUTINE read_data
!*******************
!lis les données enregistrées par l'utilisateur

USE module_reacteur_chimique
IMPLICIT NONE
LOGICAL     :: file_exists

INQUIRE(FILE='parametres.in',exist=file_exists)

IF (file_exists) THEN
    OPEN(1,FORM='FORMATTED',file='parametres.in')
    READ(1,*)
    READ(1,*)
    READ(1,*)dt !pas de temps
    READ(1,*)tfinal !temps final
    READ(1,*)nptx !nombre de points en x
    READ(1,*)npty !nombre de points en y
    READ(1,*)
    READ(1,*)l !longueur du domaine
    READ(1,*)a !paramètre A amplitude
    READ(1,*)alpha_a
    READ(1,*)alpha_b
    CLOSE(1)
ELSE
    PRINT*,' le fichier parametres.in n a pas été trouvé'
    STOP
END IF


END SUBROUTINE read_data

!******************
SUBROUTINE maillage
!******************

USE module_reacteur_chimique

IMPLICIT NONE
INTEGER         :: i, j
REAL(KIND=8)    :: dy, dx

dx = L/(nptx-1.d0)
dy = L/(npty-1.d0)
xnoeuds(1,1) = -L/2.d0
ynoeuds(1,1) = -L/2.d0

!Noeuds
DO i=2,nptx
    xnoeuds(i,1) = xnoeuds(i-1,1)+dx
END DO

DO j=2,npty
    ynoeuds(1,j) = ynoeuds(1,j-1)+dy
END DO

!Centre des Volumes
!Donc dx/2 en partant de chaque noeud en x et y
!npt-1 sur les 2
DO i=1,nptx-1
    xcentre_vol(i,1) = xnoeuds(i,1) + dx/2.d0
END DO

DO j=1,npty-1
    ycentre_vol(1,j) = ynoeuds(1,j) + dx/2.d0
END DO

!Faces
!On veut le centre des faces

!Faces horizontales
!egales aux noeuds en x + dx/2 i.e. abcisse des centre des faces ->npt-1
!meme coordonnées en y que les noeuds -> npt
xcentre_faces_horiz(:,1)=xcentre_vol(:,1)
ycentre_faces_horiz(1,:)=ynoeuds(1,:)

!Faces Verticales
!egales aux noeuds pour les abcisses -> npt
!egales ordonnées des centres ->npt-1
xcentre_faces_vertic(:,1)=xnoeuds(:,1)
ycentre_faces_vertic(1,:)=ycentre_vol(1,:)


END SUBROUTINE maillage

!***********************
SUBROUTINE champ_vitesse
!***********************

USE module_reacteur_chimique

IMPLICIT NONE
INTEGER     :: i

DO i=1,nptx
    U(i,1) =  a*DCOS(pi*((xnoeuds(i,1)/L) -0.5d0))*DSIN(pi*((ynoeuds(1,i)/L) -0.5d0)) !Ux
    U(1,i) = -a*DSIN(pi*((xnoeuds(i,1)/L) -0.5d0))*DCOS(pi*((ynoeuds(1,i)/L) -0.5d0)) !Uy

END DO

END SUBROUTINE champ_vitesse


!**************************
SUBROUTINE affichage_sortie
!**************************

USE module_reacteur_chimique
IMPLICIT NONE
CHARACTER(LEN=30)           :: nom
INTEGER                     :: i=1
CHARACTER(LEN=5)            :: tmp

! je met un nom de fichier de sortie dont le nom indique le choix de la fonction
! et le nombre de point

! je suis obligé de ruser avec les longueurs des chaines de caracteres
! car je suis obligé de définir carac d'une longueur constante
!tmp=carac(5,typ)
nom='reacteur.out'


OPEN(1,FORM='FORMATTED',FILE=TRIM(nom))
WRITE(1,100)
100 FORMAT(3x,'i',3x,'x noeuds',7x,'y noeuds',7x,'x centre Vol',7x,'y centre Vol',4x,'Vitesse x',7x,'Vitesse y',4x,'x ctr faces horiz',4x,'y ctr faces horiz')

DO i=1,nptx
    WRITE(1,200)i,xnoeuds(i,1),ynoeuds(1,i),xcentre_vol(i,1),ycentre_vol(1,i),U(i,1),U(1,i),xcentre_faces_horiz(i,1),ycentre_faces_horiz(1,i)
END DO

200 FORMAT(i4,2x,10(e13.6,3x))
CLOSE(1)

END SUBROUTINE affichage_sortie
