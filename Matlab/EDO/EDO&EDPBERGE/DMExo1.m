clear all
close all

%% Exercice 1 : Demande Biochimique en Oxygène 

%On utilise les variables globales afin qu'elles soient prises en compte
%dans la fonction D(t)
global Lo Dosat Kd Kr Do V 

%Données Numériques
n = 1000 ;%nombres de points
dt = 0.01; %pas de temps

%Paramètres
Lo = 10.9;
Dosat = 9.1;
Kd = 0.3;
Kr = 0.41;
Do = 7.6;
V=0.3;

%Initialisation
t = linspace(0,dt*n,n);
Deficit=zeros(1,n); %D : Deficit en Oxygène
DOsat=zeros(1,n); %DOSat : Saturation du Dioxygène
BOD=zeros(1,n); %BOD Demande Biochimique en Oxygène
DO=zeros(1,n); %Demande en Oxygène

for i=1:n
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

%Tracé de Figure%
figure(1).Name = "Demande Biochimique en Oxygène dans un cours d'eau";
plot(t,Deficit,t,DOsat,t,DO,t,BOD)
hold on
grid on
title('Demande Biochimique en Oxygène')
xlabel('t en jours')
ylabel('en mg/L')
legend('D','DOSat','DO','BOD')
hold off;


%% Exercice 2 : Ecosystème Dynamique Goemons-Patelles

%On utilise les variables globales afin qu'elles soient prises en compte
%dans les fonctions
global k1 k2 k3 k4 k5 beta
    k1 = 1.1;
    k2 = 1e-5;
    k3 = 1e-3;
    k4 = 0.9;
    k5 = 1e-4;
    beta = 0.02;
    
%Données Numériques
t_fin = 50;


%Conditions Initales
Goemonts_0 = [1000, 5000, 30000, 10000];
Patelles_0 = [0,50,300,1000];

figure(4).Name = "Point d'inflexion";
%solution=zeros(1,4);

for i=1:length(Goemonts_0)
    % On envoit à chaque fois les conditions initiales
    [temps, solution] = solveur(Patelles_0(i), Goemonts_0(i), t_fin);       

colors = ['r' , 'g', 'b', 'm'];
marker = ['x', 'o', '+' ,'d'];

hold(subplot(2,1,1), 'on')
figure(2).Name = "Evolutions des Proies et des Prédateurs au cours du Temps";
%Goemonts - Temps
plot(temps, solution(:,1),colors(i)) %Y(1) définit les Goemonts
grid on
ylabel('Goemonts')
xlabel('Temps (s)')
hold(subplot(2,1,1), 'off')

%Patelles - Temps
hold(subplot(2,1,2), 'on')
plot(temps, solution(:,2),colors(i)) %Y(2) définit les Patelles
grid on
ylabel('Patelles')
xlabel('Temps (s)')
hold(subplot(2,1,2), 'off')


end


hold(figure(4),'on')
plot(solution(:,2), solution(:,1));
%plot(Patelles_0(i), Goemonts_0(i),marker(i))
grid on
xlabel('Goemonts')
ylabel('Patelles')
hold(figure(4),'off')

%% Fonctions 

function [out]=D(t)
%Fonction pour Exercice 1 qui permet de définir le Déficit en Oxygène
global Lo Kd Kr Do %definies comme globales
%Input : temps courant
%Output : scalaire valeur de D au temps courant

out=(Kd*Lo)*(exp(-t*Kd)-exp(-Kr*t))/(Kr-Kd) + (Do)*exp(-Kr*t);
end

function [temps, solution] = solveur(Patelles_0, Goemonts_0, t_fin)

[temps,solution] = ode45(@(temps,Y)systeme(temps,Y), [0, t_fin], [Goemonts_0, Patelles_0]);
end


function out = systeme(temps, Y)
global k1 k2 k3 k4 k5 beta
    out = zeros(2,1);%initialisation d'un vecteur dimensions 2*1
    out(1) = Y(1) * (k1 - k2*Y(1) - k3*Y(2));
    out(2) = Y(2) * (beta*k3*Y(1) - k4 - k5*Y(2));
end



