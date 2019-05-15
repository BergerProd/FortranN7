function [z,res,err]=td2exo4(x0,e0,n0)

if nargin<3
    n0=10;
end

x1=x0;
i=1;
e=100;
while (e>e0) & (i<n0)
    x2=x1-td2exo1fun(x1)/dfdx(x1);
    e=abs(x2-x1);
    z(i)=x2;
    res(i)=td2exo1fun(x2);
    err(i)=e;
    i=i+1;
    x1=x2;
end


function out=dfdx(x)
out=tan(x)+x/cos(x)^2;
