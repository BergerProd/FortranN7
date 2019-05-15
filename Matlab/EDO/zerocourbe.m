%Script illustrant la méthode de Newton (associé à zerofun)
close all
dx=1e-3
x=pi/2:dx:1.5*pi;
y=x.*tan(x)-1;
plot(x,y)
axis([pi/2 1.5*pi -10 10])
grid on
hold on

[x0,res,err,X]=zerofun(2,1e-6,10)

Y=X.*tan(X)-1;

plot(X,Y,'-or')
plot(X(end),Y(end),'ok')



