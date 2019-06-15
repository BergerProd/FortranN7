MODULE module_reacteur_chimique

IMPLICIT NONE

INTEGER                                 :: nptx,npty,npttemps
REAL(KIND=8)                            :: dt, tfinal, l, a, alpha_a, alpha_b,pi,dx,dy,cfl,fourier,alpha_moy,Ta,Tb,T0
REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: xnoeuds, ynoeuds, xcentre_vol,ycentre_vol
REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: xcentre_faces_horiz,ycentre_faces_horiz,xcentre_faces_vertic,ycentre_faces_vertic
REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: Temp
REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: ux_centres_vol,uy_centres_vol,ux_centres_faces,uy_centres_faces
REAL(KIND=8),ALLOCATABLE,DIMENSION(:,:) :: flux_adv_x,flux_adv_y,flux_diff_x,flux_diff_y
REAL(KIND=8),ALLOCATABLE,DIMENSION(:)   :: TfaceAC,TfaceBD

END MODULE module_reacteur_chimique
