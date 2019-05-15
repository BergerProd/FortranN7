function [ xout,res,err, Xout ] = zerofun( x0,pr,N )
%calcul un zero de x*tan(x)-1 par algo Newton
%   x0: point de départ
%   pr: precision 
%   N: nbe itération max 
%   xout: zero trouvé
%   res: résidu
%   err: erreur sur x0
%   Xout: tableau des zéros itérés

% %BASIQUE
% x1=x0;
% for i=1:N
%     if abs(fun(x1))>pr
%         x0=x1;
%         x1=x0-fun(x0)/dfun(x0);
%     end
% end
% 
% xout=x1;
% res=fun(x1);
% err=abs(x1-x0);
% Xout=[];
% %FIN ALGO BASIQUE


%VARIANTE AVEC TABLEAU
x=zeros(1,N);
x(1)=x0;
k=0;
for i=1:N-1
    if abs(fun(x(i)))>pr & k==0 %test sur k nécessaire pour éviter de continuer à calculer x(i+1)
        x(i+1)=x(i)-fun(x(i))/dfun(x(i));
        iz=i+1; %c'est l'indice de x(i) pour lequel la méthode a convergé
    else
        k=1; 
    end
end

xout=x(iz);
res=fun(x(iz));
err=abs(x(iz)-x(iz-1));
Xout=x(1:iz);
%FIN VARIANTE AVEC TABLEAU

% %VARIANTE avec while
% x=zeros(1,N);
% x(1)=x0;
% i=1;
% while abs(fun(x(i)))>pr & i<N
%     x(i+1)=x(i)-fun(x(i))/dfun(x(i));
%     i=i+1;
% end
% 
% xout=x(i);
% res=fun(x(i));
% err=abs(x(i)-x(i-1));
% Xout=x(1:i);
% %FIN VARIANTE avec while


end

function y=fun(x)
y=x*tan(x)-1;
end

function y=dfun(x)
y=tan(x)+x*(1+tan(x)^2);
end


