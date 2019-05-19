%Exercice 3 du TP Probabilités
%Mars 2019
%Simulation d'une loi exponentielle

clear all
close all

%% Initialisation

N=100000;
Nc=50;
lambda = 0.5;
U=zeros(1,N);
U=rand(1,N);

%Méthode histogramme des fréquences normalisées

X=-log(1-U)/lambda; %Remplissage du vecteur


% dx = (max(X)-min(X))/Nc;
% xbins = min(X)+dx/2:dx:max(X)-dx/2; %Construction du vecteur xbins
% 
% Xsort=sort(X);
% ddpX=hist(Xsort,Nc)/(N*dx);

[ddpX,xbins,dx]=ddpxbins(N,Nc,X);% appel de la fonction


%% Figure
figure(4); hold on;
bar(xbins,ddpX,'c');
grid on
title('Simulation d''une loi Exponentielle')
xlabel('x')
ylabel('Densité de probabilité de f(x)'); 
hold off;

%% DDP loi expo 

% Loi exponentielle
for i =1:length(xbins)
    loiexp(i)=lambda*exp(-lambda*xbins(i));
end

%% Tracé Comparaison
figure(5)
hold on;
plot(xbins,loiexp,'r x -')
plot(xbins,ddpX)
grid on
title('Simulation loi exponentielle')
xlabel('x')
ylabel('Densité de probabilité de f(x)')
legend('ddploiexp','ddphistofreqnorm'); 
hold off;



