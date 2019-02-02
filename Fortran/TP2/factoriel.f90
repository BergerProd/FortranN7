program factoriel
! calcul du factoriel
! avec plusieurs précisions
! Christophe Airiau, Nov 2017
implicit none
! déclaration des variables
integer             :: n
integer             :: i
integer             :: fact_int1 =1         ! entier simple précision
integer(kind=8)     :: fact_int2 =1         ! entier double précision
real                :: fact_real1=1.0       ! réel   simple précision
real(kind=8)        :: fact_real2=1.d0      ! reél   double précision
integer(kind=16)    :: fact_int4 =1         ! entier double précision

! main
print*,'Calcul du factoriel'
print*,'entrer un nombre entier positif'
read*,n



do i=1,n
    fact_int1  = i*fact_int1*i
    fact_int2  = int(i,kind=8)*fact_int2
    fact_int4  = int(i,kind=16)*fact_int4
    fact_real1 = float(i)*fact_real1
    fact_real2 = dfloat(i)*fact_real2
end do

print*,'le factoriel est '

print*,'Entier simple précision :'
print*,fact_int1

print*,'Entier double précision :'
print*,fact_int2

print*,'Entier quadruple précision :'
print*,fact_int4

print*,'réel simple précision : '
print*,fact_real1
print'(f50.0)',fact_real1

print*,'réel double précision : '
print*,fact_real2
print'(f50.0)',fact_real2


print*,'erreur relative int2/real1  = ',float(fact_int2)/fact_real1-1.0
print*,'erreur relative real1/real2 = ',real(fact_real1,kind=8)/fact_real2-1.d0

print*
print*,"CONCLUSION : Il faut faire attention à ce qu'on déclare et à la précision de l'ordinateur"
print*,"sinon on obtient n'importe quoi ..."
print*," sur les très grands nombres ou bien sur les très petis nombres"
end program factoriel
