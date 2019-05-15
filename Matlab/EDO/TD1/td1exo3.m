%Opérations élémentaires et scripts : exo3

clear all
xm=2;N=20;
x=linspace(-xm,xm,2*N);
[X,Y]=meshgrid(x,x);
Z=X.*exp(-(X.^2+Y.^2));

close all
figure(1)
surf(X,Y,Z)
xlabel('x')
ylabel('y')
zlabel('z')

figure(2)
mesh(X,Y,Z)
xlabel('x')
ylabel('y')
zlabel('z')

figure(3)
contour(X,Y,Z,20)
xlabel('x')
ylabel('y')
dx=xm/N;
[FX,FY]=gradient(Z,dx);
hold on
quiver(X,Y,FX,FY,2)

figure(5)
mesh(X,Y,Z)
hold on
contour3(X,Y,Z,20)
xlabel('x')
ylabel('y')
