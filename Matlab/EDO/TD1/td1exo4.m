%Opérations élémentaires et scripts : exo4

clear all
t0=0;t1=1;
t=linspace(t0,t1,10);
A0=2;B0=5;
y=A0*exp(-B0*t);

s=0.2;
%rng('default');
y=y.*(1+s*randn(1,10))

close all
figure(1)
plot(t,y,'o')
hold on

x=log(y);
D=[ones(10,1) t'];
C=D\x';

A=exp(C(1))
B=-C(2)

T=linspace(t0,t1,20);
Y=A*exp(-B*T);
plot(T,Y,'-')
legend('exp','y=A e^{-Bt}')
xlabel('t'),ylabel('y')
grid on
