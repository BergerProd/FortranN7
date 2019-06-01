module module_VF

IMPLICIT NONE

INTEGER					::i,j,k				!Variables utilisées pour les boucles
INTEGER					::Nx,Ny,Nt			!Nombre de noeuds en x et y du maillage
REAL(KIND=8)				::L				!Longueur du domaine
REAL(KIND=8)				::A				!Paramètre de l'éq. de la vitesse
REAL(KIND=8)				::alpha_a=0,alpha_b		!Coeffs. de diffusivité thermique
REAL(KIND=8)				::x1,x2,y1,y2			!Bornes des intervalles de maillage
REAL(KIND=8)				::dy,dx				!Pas du maillage
REAL(KIND=8)				::Ta,Tb,TO			!Constantes de température
REAL(KIND=8)				::sigmaA,sigmaB			!Constantes = L/20
REAL(KIND=8)				::CFL				!Nombre de courant (optimal pour CFL=1)
REAL(KIND=8)				::r				!Nombre de Fourier (optimal pour r=0,5)
REAL(KIND=8)				::dt				!Pas de temps 
REAL(KIND=8)				::Tf				!Temps de fin d'observation
REAL,DIMENSION(:),ALLOCATABLE		::X,Y				!Vecteur des coordonnées de chaque maille
REAL,DIMENSION(:,:),ALLOCATABLE		::tab_x,tab_y			!Tableaux des noeuds de maillage
REAL,DIMENSION(:,:),ALLOCATABLE		::tab_xc,tab_yc			!Tableaux des centres de maillage
REAL,DIMENSION(:),ALLOCATABLE		::T_A,T_B			!Tableau contenant l'eq. de Tempérture
REAL,DIMENSION(:,:),ALLOCATABLE		::dt_loc			!Tableau de pas de temps local
REAL,DIMENSION(:,:),ALLOCATABLE		::T				!Tableau de la température
REAL,DIMENSION(:,:),ALLOCATABLE		::U,V,Uo			!Tableau des composantes des vitesses U,V
REAL,DIMENSION(:,:),ALLOCATABLE		::flux_adv_X,flux_adv_Y		!Flux advectifs totaux respectivement sur X et Y
REAL,DIMENSION(:,:),ALLOCATABLE		::flux_diff_X,flux_diff_Y	!Flux diffusif en haut d'une maille donnée


end module module_VF
