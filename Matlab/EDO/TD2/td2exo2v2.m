function out=td2exo2v2(x)
global a n
%attention : a et n ont du �tre declar�s tout d'abord 'global' dans la fen�tre
%de commande, puis initialis�s (voir script td2exo2sc.m)
out=((a-abs(x)).*cos(n*x)).*((x<=a)&(x>=-a));
end
