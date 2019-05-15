function [ddp,xbins,dx]=ddpxbins(N,Nc,X)
%Fonction qui sert à générer la ddp, elle est utilisée dans les exercies
%1,2 et 3
%Input : N nombre d'échantillons, Nc le nombre de classes et X le vecteur
%des variables aléatoires
%Output : ddp, xbins et dx le pas

dx=(max(X)-min(X))/(Nc); %Discrétisation
xbins = min(X)+dx/2:dx:max(X)-dx/2; %Construction du vecteur xbins
Xsort=sort(X); %Classement selon ordre croissant
ddp=hist(Xsort,Nc)/(N*dx);

end