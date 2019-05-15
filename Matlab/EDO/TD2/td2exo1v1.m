%Fonctions : exo1

clear all
x=linspace(-pi,pi,100);
y=td2exo1fun(x);

close all
plot(x,y)
grid on
xlabel('x'),ylabel('y')
axis([-pi pi -10 10])
hold on

x0=-1;
z=fzero(@td2exo1fun,x0)
plot(z,td2exo1fun(z),'o','MarkerSize',10)

x0=1;
z=fzero(@(x) x.*tan(x)-1,x0)
plot(z,td2exo1fun(z),'o','MarkerSize',10)
