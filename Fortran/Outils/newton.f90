program newton
    implicit none
    real(kind=8) :: x,h,a,b,eps,dx
    real(kind=8), external :: f ,df
    integer :: n,i,m,imax


    !!!!!!!!!!!!!UITILISATEUR (eventuellement .in plus tard...) !!!!!!!!!!
    !bornes
    a=0.d0
    b=2.d0
    !precision
    eps=0.00000000000001d0
    !nombre d'iteration max
    imax=20
    !nombre d'iteration de l'algo pour racine multiples:
    m=100
    !!!!!!
    !penser a changer la fonction F
    !!!!!!
    !!!!!!!!!!!!!! fin utilisateur!!!!!!!!!!!!!!!
    
    dx=(b-a)/float(m) 
    x=a   
    Do n=0,m
        i=0
        if(f(x)*f(x+dx).LE.0) then !calcul que aux endroit interessant
            DO WHILE((abs(f(x))>eps) .AND. (i<imax)) !algo de newton
                h= -f(x)/df(x)
                x=x+h
                i=i+1
            END DO
            if((x.GE.a-eps) .and. (x.LE.b+eps)) then
                print *, "Xzero = ",x,"en",i,"iterations" !print uniquement si c'est bien une racine de l'intervalle
            end if
        end if
        x=x+dx
    END DO
end program newton



real(kind=8) function f(x)
    implicit none
    real(kind=8), intent(in) :: x 
    real(kind=8) :: a=1.4d0
    
    !!!!!!!!!!!!!UITILISATEUR (eventuellement .in plus tard...) !!!!!!!!!!    
    !fonction 1
    !f = x*(x-1)*(x-2)
    !fonction 2
    !f= dcos(x)
    !fonction 3
    f= ((1-x**2) /(a*x) + dlog(((a+1)* x**2 )/ (2+(a-1)*x**2)) * (a+1)/(2*a)) -0.5
    !!!!!!!!!!!!!! fin utilisateur!!!!!!!!!!!!!!!    
    
    
    
    
end function f

real(kind=8) function df(x)
    implicit none
    real(kind=8), intent(in) :: x 
    real(kind=8) :: a=1.4d0 , dx=0.000000001d0
    real(kind=8), external :: f 
    
    
    !!!!!!!!!!!!!derivée pour verification     
    !fonction1
    !df = (x-1)*(x-2) + x*(x-2) + x*(x-1)
    !fonction 2
    !df= -dsin(x)
    !fonction 3
    !df= -(1+x**2)/(a*x**2) + (4*(a+1))/(2*a*x*((a+1)*x**2 +2))
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
    
    
    df=(f(x+dx)-f(x))/dx
    
    
    
     
end function df










