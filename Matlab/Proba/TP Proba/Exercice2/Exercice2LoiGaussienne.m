%Exercice 2 Partie 1 & 2 du TP Probabilités
%Mars 2019
%Partie 1 : Comparaison de la génération d'une loi normale selon
%l'histogramme des fréquences normalisées (ddp)
%et le générateur Matlab randn
%Partie 2 : Méthode de Box Muller

close all
clear all

%% Initialisation
N=100;
Nc=50;
X=zeros(1,N);
X=randn(1,N); %Initialisation selon loi normale

%Méthode histogramme des fréquences normalisées

% dx=(max(X)-min(X))/(Nc); %Discrétisation
% xbins = min(X)+dx/2:dx:max(X)-dx/2; %Construction du vecteur xbins
% 
% Xsort=sort(X);
% ddpX=hist(Xsort,Nc)/(N*dx);

[ddpX,xbins,dx]=ddpxbins(N,Nc,X);% appel de fonction pour Méthode histogramme des fréquences normalisées


%Pour Générateurs
sx = std(X);%écart type
mx = mean(X);%moyenne 


% normpdf
fx1 = normpdf(xbins,mx,sx);

%fx2
for i = 1:length(xbins)
    fx2(i) = (1/(sx*sqrt(2*pi)))*exp(-((xbins(i)-mx)^2)/(2*sx^2));
end

%% Tracé de la comparaison 1

figure(2);hold on;
bar(xbins,ddpX,'c')
plot(xbins,fx1,'r o -')
plot(xbins,fx2,'g x -')
grid on;
title('Comparaison de Loi Gaussienne')
xlabel('x')
ylabel('Densité de probabilités des f(x)')
legend('ddp','normpdf','fx2')
hold off;



%% Partie 2 : Méthode de Box Muller

%Initialisation des vecteurs U et V
U=rand(1,N);
V=rand(1,N);

X1 = sqrt(-2*log(U)).*cos(2*pi*V);% ici ln
Y1 = sqrt(-2*log(U)).*sin(2*pi*V); %on n'utilise pas Y finalement


dx1 =(max(X1)-min(X1))/(Nc); %Discrétisation
x1bins = min(X1)+dx1/2:dx1:max(X1)-dx1/2; %Construction du vecteur xbins

mx1 = mean(X1); %moyenne 
sx1 = std(X1); 	%ecart-type

for i = 1:length(x1bins) %on pourrait aussi le faire sans une boucle mais avec *.
    ddpx1(i) = (1/(sx1*sqrt(2*pi)))*exp(-((x1bins(i)-mx1)^2)/(2*sx1^2));
end



%% Tracé de la comparaison 2

figure(3);hold on;
plot(xbins,fx1,'r x -') %normpdf
plot(xbins,fx2,'g o -') %fx2
plot(x1bins,ddpx1,'b + -' )%BoxMuller
grid on;
title('Comparaison de Loi Gaussienne')
xlabel('x')
ylabel('Densité de probabilité des f(x)')
legend('normpdf','fx2','BoxMuller')
hold off;