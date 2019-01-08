!********************
SUBROUTINE read_data
!********************
!lecture du fichier d'entrée profil.in

USE module_tp4
IMPLICIT NONE
LOGICAL     :: file_exists

!vérification si fichier existant
INQUIRE(FILE='profil.in',exist=file_exists)

IF (file_exists) THEN
    OPEN(1,FORM='FORMATTED',file='profil.in')
    READ(1,*)
    READ(1,*)option !option permettant de calculer NACA4 ou NACA5
    READ(1,*)a
    READ(1,*)b
    READ(1,*)c
    READ(1,*)d
    READ(1,*)e
    READ(1,*)x1
    READ(1,*)x2
    READ(1,*)npt
    CLOSE(1)
ELSE
    PRINT*,' le fichier profil.in n''a pas été trouvé'
    STOP
END IF

END SUBROUTINE read_data

!*******************
SUBROUTINE maillage
!******************
!maillage sur la longeur de la corde
USE module_tp4
IMPLICIT NONE

INTEGER         :: i
REAL(KIND=8)    :: dx

! Maillage régulier
dx=(x2-x1)/DFLOAT(npt-1)
x(1)=x1
DO i=2,npt
    x(i)=x(i-1)+dx
END DO

END SUBROUTINE maillage


!******************************
SUBROUTINE cambrure_simple_NACA5
!******************************
!calcul de g,h et df dans le cas d'une cambrure simple en NACA5
!ces vecteurs sont définis comme variables globales dans le module
USE module_tp4

IMPLICIT NONE
INTEGER                   :: i
REAL(KIND=8)              :: ligne,k1
REAL(KIND=8),DIMENSION(5) :: ligne_camb_smpl,p_camb_smpl,m_camb_smpl,k1_camb_smpl


!vecteurs contenant les données pour le tableau cas cambrure_simple_NACA5
!on copiera simplement alors la valeur avec l'indice pour le vecteur que l'on affectera à la variable correspondante
ligne_camb_smpl = (/210.d0,220.d0,230.d0,240.d0,250.d0/)
p_camb_smpl = (/5.d-2,1.d-1,1.5d-1,2.d-1,2.5d-1/)
m_camb_smpl = (/0.0580d0,0.126d0,0.2025d0,0.290d0,0.391d0/)
k1_camb_smpl = (/361.4d0,51.640d0,15.957d0,6.643d0,3.320d0/)

!initialisation des valeurs
m=0.d0
p=0.d0
k1=0.d0
l=x2-x1
t=(10.d0*DFLOAT(d)+DFLOAT(e))/(100.d0)
ligne = 100.d0*DFLOAT(a)+10.d0*DFLOAT(b)+DFLOAT(c)
!astuce pour aller comparer avec la ligne de cambrure selon les données a,b,c

!étant donné que ce sont des réels, on peut ne pas avoir égalité parfaite
!donc on effectue une comparaison avec un epsilon défini à 1e-15
IF (abs(ligne - ligne_camb_smpl(1)) .LE. 1d-15 ) THEN
  p = p_camb_smpl(1)
  m = m_camb_smpl(1)
  k1 = k1_camb_smpl(1)
ELSEIF (abs(ligne - ligne_camb_smpl(2)) .LE. 1d-15 ) THEN
  p = p_camb_smpl(2)
  m = m_camb_smpl(2)
  k1 = k1_camb_smpl(2)
ELSEIF (abs(ligne - ligne_camb_smpl(3)) .LE. 1d-15 ) THEN
  p = p_camb_smpl(3)
  m = m_camb_smpl(3)
  k1 = k1_camb_smpl(3)
ELSEIF (abs(ligne - ligne_camb_smpl(4)) .LE. 1d-15 ) THEN
  p = p_camb_smpl(4)
  m = m_camb_smpl(4)
  k1 = k1_camb_smpl(4)
ELSEIF (abs(ligne - ligne_camb_smpl(5)) .LE. 1d-15 ) THEN
  p = p_camb_smpl(5)
  m = m_camb_smpl(5)
  k1 = k1_camb_smpl(5)
ELSE
  WRITE(*,*) "Mauvaise saisie de abc pour NACA5, erreur dans cambrure_simple_NACA5"
ENDIF

DO i=1,npt
  eta = x(i)/l
  g(i) = (l*t)/(0.2d0)*( (a0*(eta**0.5)) - &
  (a1*eta)+(- a2*(eta**2))+ &
  (+ a3*(eta**3))+( -a4*(eta**4)) )

  IF ((0 .LE. eta).AND.(eta .LT. m)) THEN
    h(i)  = l*(k1/(6.d0))*(eta**3 - (3.d0)*m*(eta**2) + (m**2)*(3.d0-m)*eta)
    df(i) = (k1/(6.d0))*((3.d0)*(eta**2) - (6.d0)*m*eta + (m**2)*(3.d0-m))

  ELSEIF ((m .LE. eta).AND.(eta .LE. 1)) THEN
    h(i)  = ((k1*(m**3))/(6.d0))*(1.d0-eta)
    df(i) = (-k1*(m**3))/(6.d0)

  ELSE
    WRITE(*,*) "eta = ", eta, "ne correspond à aucun des cas, &
    & doit etre compris entre 0 et 1, vérifiez x1 et x2, erreur dans cambrure_simple_NACA5"
  ENDIF
END DO

END SUBROUTINE cambrure_simple_NACA5

!*******************************
SUBROUTINE cambrure_double_NACA5
!******************************
!calcul de g,h et df dans le cas d'une cambrure double en NACA5
!ces vecteurs sont définis comme variables globales
USE module_tp4

IMPLICIT NONE
INTEGER                   :: i
REAL(KIND=8)              :: ligne,k1,k2_k1
REAL(KIND=8),DIMENSION(4) :: ligne_camb_dbl,p_camb_dbl,m_camb_dbl,k1_camb_dbl,k2_k1_camb_dbl


!vecteurs contenant les données pour le tableau cas cambrure_double_NACA5
!on copiera simplement alors la valeur avec l'indice pour le vecteur que l'on affectera à la variable correspondante
ligne_camb_dbl = (/221.d0,231.d0,241.d0,251.d0/)
p_camb_dbl = (/0.10d0,0.15d0,0.20d0,0.25d0/)
m_camb_dbl = (/0.130d0,0.217d0,0.318d0,0.441d0/)
k1_camb_dbl = (/51.990d0,15.793d0,6.520d0,3.191d0/)
k2_k1_camb_dbl = (/0.000764d0,0.00677d0,0.0303d0,0.1355d0/)

m=0.d0
p=0.d0
k1=0.d0
k2_k1=0.d0
l=x2-x1
t=(10.d0*DFLOAT(d)+DFLOAT(e))/(100.d0)
ligne = 100.d0*DFLOAT(a)+10.d0*DFLOAT(b)+DFLOAT(c)
!astuce pour aller comparer avec la ligne de cambrure selon les données a,b,c

!étant donné que ce sont des réels, on peut ne pas avoir égalité parfaite
!donc on effectue une comparaison avec un epsilon défini à 1e-15
IF (abs(ligne - ligne_camb_dbl(1)) .LE. 1d-15 ) THEN
  p = p_camb_dbl(1)
  m = m_camb_dbl(1)
  k1 = k1_camb_dbl(1)
  k2_k1 =k2_k1_camb_dbl(1)
ELSEIF (abs(ligne - ligne_camb_dbl(2)) .LE. 1d-15 ) THEN
  p = p_camb_dbl(2)
  m = m_camb_dbl(2)
  k1 = k1_camb_dbl(2)
  k2_k1 =k2_k1_camb_dbl(2)
ELSEIF (abs(ligne - ligne_camb_dbl(3)) .LE. 1d-15 ) THEN
  p = p_camb_dbl(3)
  m = m_camb_dbl(3)
  k1 = k1_camb_dbl(3)
  k2_k1 =k2_k1_camb_dbl(3)
ELSEIF (abs(ligne - ligne_camb_dbl(4)) .LE. 1d-15 ) THEN
  p = p_camb_dbl(4)
  m = m_camb_dbl(4)
  k1 = k1_camb_dbl(4)
  k2_k1 =k2_k1_camb_dbl(4)
ELSE
  WRITE(*,*) "Mauvaise saisie de abc pour NACA5, erreur dans cambrure_double_NACA5"
ENDIF

DO i=1,npt
  eta = x(i)/l
  g(i) = (l*t)/(0.2d0)*((a0*(eta**0.5)) - &
  (a1*eta)+(-a2*(eta**2))+ &
  (+a3*(eta**3))+(-a4*(eta**4)))

  IF ((0 .LE. eta).AND.(eta .LT. m)) THEN
    h(i)  = l*(k1/(6.d0))*( ((eta-m)**3) - k2_k1*((1.d0-m)**3)*eta - (m**3)*eta + m**3)
    df(i) = (k1/(6.d0))*(3.d0*((eta-m)**2) - k2_k1 *((1.d0-m)**3) -(m**3))

ELSEIF ((m .LE. eta).AND.(eta .LE. 1)) THEN
    h(i)  = l*(k1/(6.d0))*(k2_k1 * ((eta -m)**3) - k2_k1*((1.d0-m)**3)*eta - (m**3)*eta + (m**3))
    df(i) = (k1/(6.d0))*(3.d0*k2_k1 *((eta-m)**2) - k2_k1*((1.d0-m)**3)-(m**3))

ELSE
  WRITE(*,*) "eta = ", eta, "ne correspond à aucun des cas, &
    & doit etre compris entre 0 et 1, vérifiez x1 et x2, erreur dans cambrure_double_NACA5"
  ENDIF
END DO

END SUBROUTINE cambrure_double_NACA5


!**********************************
SUBROUTINE cambrure_NACA4
!**********************************
!Calcul de g, h et df dans le cas de NACA4
!Ces vecteurs sont définis comme variables globales
! On utilise une seul subroutine de cambrure pour NACA4 puisqu'elle peut traiter
! sans erreurs le cas ou a=b=0 et le cas ou a et b sont non nuls en même temps

USE module_tp4

IMPLICIT NONE
INTEGER                      :: i

l=x2-x1
m=(DFLOAT(a))/(100.d0)
p=(DFLOAT(b))/(10.d0)
t=(10.d0*DFLOAT(c)+DFLOAT(d))/(100.d0) !ici t différent que pour NACA5

DO i=1,npt
  eta = x(i)/l
  g(i) = (l*t)/(0.2d0)*((a0*(eta**0.5)) - &
  (a1*eta)+(-a2*(eta**2))+ &
  (a3*(eta**3))-(a4*(eta**4)))

  IF ((0 .LE. eta).AND.(eta .LT. p)) THEN
    h(i) = ((m*x(i))/((p**2)*l))*((2.d0)*p - eta)
    df(i) = (((2.d0)*m)/(p**2))*(p - eta)

  ELSEIF ((p .LE. eta) .AND. (eta .LE. l)) THEN
    h(i) = m*((l-x(i))/((1.d0-p)**2))*((1.d0) + eta - (2.d0*p))
    df(i) = ((2.d0*m)/((1.d0-p)**2))*(p-eta)

  ELSE !eta doit etre compris entre 0 et 1 au maximum donc au delà ou en dessou c'est qu'on a une erreur sur l'intervalle x1,x2
    WRITE(*,*) "eta = ", eta, "ne correspond à aucun des cas, vérifiez x1 et x2, &
    & erreur dans cambrure_NACA4"
  ENDIF
END DO

END SUBROUTINE cambrure_NACA4

!***********************************
SUBROUTINE calcul_extrados_intrados
!************************************
!calcul des abscisses et ordonnées des extrados et intrados et de f
!utilsé pour tous les cas
!toutes ces variables sont définies comme globales

USE module_tp4
IMPLICIT NONE
INTEGER     :: i

!voir différence avec méthode
!theta(:) = datan(df(:))
!Laquelle est la plus rapide et la moins complexe pour l'exécution ?

DO i=1,npt
  theta(i) = datan(df(i))
  xe(i) = x(i)-g(i)*dsin(theta(i))
  xi(i) = x(i)+g(i)*dsin(theta(i))
  ye(i) = h(i)+g(i)*dcos(theta(i))
  yi(i) = h(i)-g(i)*dcos(theta(i))
  f(i)  = (ye(i) + yi(i))/(2.d0)
END DO

END SUBROUTINE calcul_extrados_intrados

!****************************
SUBROUTINE affichage_sortie
!****************************

USE module_tp4
IMPLICIT NONE
CHARACTER(LEN=30)           :: nom
INTEGER                     :: i
CHARACTER(LEN=5)            :: tmp1,tmp2,tmp3,tmp4,tmp5


! nom de fichier de sortie dont le nom indique le profil NACA et nombre de points
! on ajoute la valeur de e si on est dans le cas de NACA 5

! je suis obligé de ruser avec les longueurs des chaines de caracteres
! car je suis obligé de définir carac d'une longueur constante
tmp1=carac(5,a)
tmp2=carac(5,b)
tmp3=carac(5,c)
tmp4=carac(5,d)
tmp5=carac(5,e)

IF (option==4) THEN !cas NACA4 on affiche uniquement abcd
  nom='tp4_NACA_'//tmp1(5:5)//tmp2(5:5)//tmp3(5:5)//tmp4(5:5)//'_npt_'//carac(5,npt)//'.out'
ELSE !cas NACA5 ou on affiche abcde
  nom='tp4_NACA_'//tmp1(5:5)//tmp2(5:5)//tmp3(5:5)//tmp4(5:5)//tmp5(5:5)//'_npt_'//carac(5,npt)//'.out'
ENDIF

PRINT*, 'Regardez dans le fichier ',nom
OPEN(1,FORM='FORMATTED',FILE=TRIM(nom))
WRITE(1,100)
100 FORMAT('#',2x,'i',2x,'x',15x,'xi',15x,'xe',15x,'yi',15x,'ye',15x,'h',15x,'f',15x,'g')
 !les 15x correspondent aux espaces entre chaque colonnes pour aligner le nom avec les valeurs dans la colonnes

DO i=1,npt
    WRITE(1,200)i,x(i),xi(i),xe(i),yi(i),ye(i),h(i),f(i),g(i)
END DO

200 FORMAT(i4,8(e13.6,3x)) !formattage pour les valeurs dans les colonnes
CLOSE(1)

END SUBROUTINE affichage_sortie
