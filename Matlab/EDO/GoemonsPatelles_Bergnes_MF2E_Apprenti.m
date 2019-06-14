%% Goemons Patelles

clc
clear
close all

%% Parametres temporels et de population

t_final = 50;
G0 = [5000, 50000, 500000, 50000];   %Vecteur population de goemon (proies)
P0 = [50, 500, 500, 5000];           %Vecteur population de patelle (prédateurs)

color = ['y','r','b','g']

%% Graphique

% Evolution des proies

for i=1:4
   [t,sol]=solver(P0(i),G0(i),t_final) 
hold on 
figure(1)
plot(t,sol(:,1),color(i))
ylabel('Nombre de proies')
xlabel('time (s)')
legend('5000,50','50000,500','500000,500','50000,5000')
title('Evolution du nombre de proies en fonction du temps')
grid on
hold off
  
% Evolution des predateurs

hold on 
figure(2)
plot(t,sol(:,2),color(i))
ylabel('Nombre de predateur')
xlabel('time (s)')
legend('5000,50','50000,500','500000,500','50000,5000')
title('Evolution du nombre de prédateurs en fonction du temps')
grid on
hold off

% Analyse de la stabilite

hold on
figure(3) 
plot(sol(:,2), sol(:,1),color(i))
ylabel('Goemons')
xlabel('Patelles')
legend('5000,50','50000,500','500000,500','50000,5000')
title('Stabilité des couples goemons patelles')
grid on
hold off
end

%% Fonctions

function [t, sol] = solver(P0, G0, tfinal)                   %Introduction des parametres
    k1 = 1.1;
    k2 = 1e-5;
    k3 = 1e-3;
    k4 = .9;
    k5 = 1e-4;
    beta = .02;
 
    [t,sol] = ode45(@(t,Y) systeme(t,Y, k1, k2, k3, k4, k5, beta), [0, tfinal], [G0, P0]);  %Resolution de l equation differentielle
end

function Equation = systeme(t, Y, k1, k2, k3, k4, k5, beta)   %Creation du systeme des deux equations
    Equation = zeros(2,1);                                    %Initialisation du vecteur
    Equation(1) = Y(1) * (k1 - k2*Y(1) - k3*Y(2));            %Premiere equation
    Equation(2) = Y(2) * (beta*k3*Y(1) - k4 - k5*Y(2));       %Deuxieme equation
end