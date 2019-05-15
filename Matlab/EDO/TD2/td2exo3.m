%Fonctions : exo3
clear all
global a n
a=5;
n=10;
x=linspace(-5,5,200);
y=td2exo2v2(x);

close all
plot(x,y)
grid on
xlabel('x'),ylabel('y')

I=quad(@td2exo2v2,-5,5)

N=0:0.2:10;
Iq=zeros(size(N));
It=Iq;
dx=(x(end)-x(1))/length(x);

for i=1:length(N)
    n=N(i);
    Iq(i)=quad(@td2exo2v2,-5,5);
    y=td2exo2v2(x);
    It(i)=trapz(x,y);
end

figure
plot(N,Iq,'-or',N,It,'-^c')

legend('quad','trapz 200 points')
