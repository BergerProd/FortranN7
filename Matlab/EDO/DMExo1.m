clear all
close all

%% Exercice 1 : Demande Biochimique en Oxyg�ne 

%On utilise les variables globales afin qu'elles soient prises en compte
%dans la fonction D(t)
global Lo Dosat Kd Kr Do V 
t = linspace(0,20,10000);

Lo = 10.9;
Dosat = 9.1;
Kd = 0.3;
Kr = 0.41;
Do = 7.6;
V=0.3;


Deficit(1)=0; %D
DOsat(1)=0; %DoSat
BOD(1)=0;
DO(1)=0;

for i=1:10000
   Deficit(i)=D(t(i));
   DOsat(i)=Dosat;
   BOD(i)= Lo*exp(-Kd*t(i));
end    

DO = -Deficit + DOsat; %Construction de DO


%Calcul de tmax

A =( -Kd^2 *Lo )/(Kr-Kd);

B = (Kd*Lo*Kr)/(Kr-Kd);

C = Do*Kr;

tmax = log((C-B)/A) / (Kr - Kd);
xmax = tmax*V*3600*24;
Deficit_tmax=D(tmax);

%Trac� de Figure%
plot(t,Deficit,t,DOsat,t,DO,t,BOD)
hold on
grid on
title('Demande Biochimique en Oxygène')
xlabel('t en jours')
ylabel('en mg/L')
legend('D','DOSat','DO','BOD')
hold off;


%% Exercice 2 : Ecosyst�me Dynamique Goemons-Patelles


%% Fonctions 
function [out]=D(t)
%Fonction pour Exercice 1 qui permet de d�finir le D�ficit en Oxyg�ne
global Lo Kd Kr Do %definies comme globales
%Input : temps courant
%Output : scalaire valeur de D au temps courant

out=(Kd*Lo)*(exp(-t*Kd)-exp(-Kr*t))/(Kr-Kd) + (Do)*exp(-Kr*t);
end



