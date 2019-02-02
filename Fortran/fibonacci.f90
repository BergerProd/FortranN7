program suite_fibonacci
! Christophe Airiau, Octobre 2013
! Calcul de la suite de Fibonacci
!
implicit none
integer             :: n   ! nombre de termes de la suite
integer             :: nmax=30   ! nombre de termes maximal de la suite
integer             :: u1=1,u2=1 ! Deux premiers termes de la série initialisés
integer             :: u3   ! terme courant u_n
integer             :: i    ! indice de la boucle

print*,' Suite de Fibonacci'
print*,' Entrer un entier pas trop grand, nombre de termes de la suite :'
read*,n

! Petite vérification sur la valeur de l'entrée
! ce n'est pas obligatoire, mais sécurise le programme.
if ((n.gt.nmax).or.(n.lt.0)) then
    print*,'entrer un entier positif, inférieur à ', nmax
    stop
end if
! calcul du terme courant
do i=2,n
    u3=u2+u1
    print*,u1,' + ',u2,' = ',u3 ! affichage de l'opération et du résultat
    u1=u2; u2=u3 ! préparation de l'itération suivante
end do

end program suite_fibonacci
