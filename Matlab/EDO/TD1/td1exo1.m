%Opérations élémentaires et scripts : exo1

x1=1e-2;
x2=2;
N=1000;

x=linspace(x1,x2,N);
y=sin(1./x);

close all
figure(1)
plot(x,y)
grid on
xlabel('x')
ylabel('y=sin (1/x)')

x1=-2;
x2=0;
N=1000;
x=logspace(x1,x2,N);
y=sin(1./x);

figure(2)
plot(x,y)
grid on
xlabel('x')
ylabel('y=sin (1/x)')

figure(3)
semilogx(x,y)
grid on
xlabel('x')
ylabel('y=sin (1/x)')
