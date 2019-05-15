function out=td2exo2v2(x)
global a n
%attention : a et n ont du être declarés tout d'abord 'global' dans la fenêtre
%de commande, puis initialisés (voir script td2exo2sc.m)
out=((a-abs(x)).*cos(n*x)).*((x<=a)&(x>=-a));
end
