%Exercice 4 Tp de Probabilités
%Mars 2019
%Simulation d'une loi de Bernouilli

clear all
close all

%initialisation
N = 1000000; %Nombre d'échantillons
p = 0.8;%Probabilité du succès

X = rand(1,N);
NCF = 0;%initialisation du compteur nombre de cas favorables

for i=1:N
    if (X(i) <= p) % si on est en succés on incrémente le compteur
        NCF = NCF + 1;
    end
end

P = NCF/N;%probabibilité : nommbre de cas favorables sur nombre d'échantillons

fprintf('%0.10f',P) %affichage à l'écran
