function [x,res,err] = Newton(f,xzero,eps,nbiter)


i=1;
err=1;
x(1)=xzero;

while (i<=nbiter)&&(err>eps)
    derivee = (f(x(i)+eps)-f(x(i)))/eps;
    x(i+1)=x(i)-(f(x(i))/derivee) ;
    err=abs(x(i+1)-x(i));
    i=i+1;
end

res = f(x(i));

end

 
    
    
    
