%Fonctions : exo2 : script utilisant td2exo2v2.m

clear all
global a n
a=2;
n=10;
x=-5:0.01:5;
y=td2exo2v2(x);

close all
plot(x,y)
