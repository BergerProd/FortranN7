program calcul_pi
! calcul du nombre pi
implicit none
real(kind=8)    :: Pi,pi2,pi_ref,err
real(kind=8)    :: eps
real            :: Pi_sp
integer         :: n=10
integer         :: i,choix

Print*,'Programme du calcul du nombre Pi par différents algorithmes'
Print*,'Menu '
print*,' <1> : calcul simple et double précision, 1 arc tangente '
print*,' <2> : calcul double précision,           2 arc tangentes'
print*,' <3> : formule de BBP, N fixé = ',n
print*,' <4> : formule de BBP, eps à fixer, N calculé'
print*,'entrer votre choix '
read*,choix

! formule double précision
Pi_ref=4.d0*datan(1.d0)
print*, 'Pi de référence                ',pi_ref
print'(a,e30.23)', 'Pi de référence     ',pi_ref

select case (choix)
    case (1)
        ! formule simple précision
        Pi_sp=4*atan(1.)
        print*, 'Pi simple précision            ',pi_sp
        print'(a,e30.23)', 'Pi simple précision ',pi_sp
        print'(a,e30.23)', 'erreur              ',pi_sp/pi_ref-1.d0

    case (2)
        ! autre formule d'une très grande précision
        Pi = 16.d0*datan(1.d0/5.d0) - 4.d0 * datan (1.d0/239.d0)
        print'(a,e30.23)', 'Pi formule 2     ',pi
        print'(a,e30.23)', 'erreur formule 2 ',pi/pi_ref-1.d0

    case (3)
        ! Bailey–Borwein–Plouffe formula
        ! calcul avec beaucoup de décimales
        pi2=0.d0
        do i=0,n
            pi2=pi2+1.d0/16.d0**i*(         &
               4.d0/(8.d0*dfloat(i)+1.d0)  -   &
               2.d0/(8.d0*dfloat(i)+4.d0)  -  &
               1.d0/(8.d0*dfloat(i)+5.d0)  -  &
               1.d0/(8.d0*dfloat(i)+6.d0))
        end do

        print*,' Formule de Bailey-Borwein-Plouffe '
        print'(a,e30.23)', 'Pi formule 3     : ',pi2
        print'(a,e30.23)', 'erreur formule 3 : ',pi2/pi_ref-1.d0


    case (4)
    print*,'entrer une erreur recherchée : '
    read*,eps
    print*, 'erreur recherchée : ', eps
    pi2=0.d0
    err=1.d0
    i=0
    do while ((err.gt.eps).and.(i.lt.10))
        pi2=pi2+1.d0/16.d0**i*(         &
           4.d0/(8.d0*dfloat(i)+1.d0)  -   &
           2.d0/(8.d0*dfloat(i)+4.d0)  -  &
           1.d0/(8.d0*dfloat(i)+5.d0)  -  &
           1.d0/(8.d0*dfloat(i)+6.d0))
         i=i+1
         err=abs(pi2/pi_ref-1.d0)
         print'(6x,i4, 3x,e30.23) ',i, err
    end do

    print'(a,e30.23)', 'Pi formule 3     : ',pi2
    print'(a,e30.23)', 'erreur formule 3 : ',err
    print*,'N =    ',i-1



    case DEFAULT
        print*, 'mauvaise option'
        STOP
end select








end program calcul_pi
