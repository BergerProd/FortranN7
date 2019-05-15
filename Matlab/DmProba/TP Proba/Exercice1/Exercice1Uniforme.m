%Exercice 1 du TP Probabilités
%Mars 2019
%Comparaison de la génération d'une loi uniforme selon un générateur
%congruentiel avec la méthode de l'histogramme des fréquences normalisées
%et le générateur Matlab rand

clear all
close all

%% Question 1

N0 = 1; %Premier Echantillon
Nc = 50; %Nombre de classes 
N = 10000000; %Nombre d'échantillons

% Initialisation
Ni=zeros(1,N);
U=zeros(1,N);
Ni(1)=1;

L=1027;
C=0;
M=2^20;

for i=N0+1:N
    Ni(i)= mod((L*Ni(i-1)+C),M);
end    
U=(Ni/M);

%% Question 2
%Méthode histogramme des fréquences normalisées
%On peut le faire directemnt ici ou on fait appel à une fonction

% dx=(max(U)-min(U))/(Nc); %Discrétisation
% xbins = min(U)+dx/2:dx:max(U)-dx/2; %Construction du vecteur xbins
% 
% Usort=sort(U);
% ddpU=hist(Usort,Nc)/(N*dx);

[ddpU,xbins,dx]=ddpxbins(N,Nc,U);% appel de la fonction

%% Générateur Matlab rand
Um=rand(1,N);
Umsort=sort(Um);
ddpUm=hist(Umsort,Nc)/(N*dx);

%% Tracé de la comparaison

figure(1); hold on;
bar(xbins,ddpU,'c');
plot(xbins,ddpUm,'r');
grid on
title('Densité de probabilité d''une loi uniforme 1⁷ échantillons')
xlabel('x')
ylabel('Densité de probabilité de f(x)');
legend ('Générateur Congruentiel','Générateur Matlab')
hold off;


