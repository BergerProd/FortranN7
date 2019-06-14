%% Exercice 2 : Ecosyst�me Dynamique Goemons-Patelles

clear all
close all

%On utilise les variables globales afin qu'elles soient prises en compte
%dans les fonctions
global k1 k2 k3 k4 k5 beta
    k1 = 1.1;
    k2 = 1e-5;
    k3 = 1e-3;
    k4 = 0.9;
    k5 = 1e-4;
    beta = 0.02;
    
% %Donn�es Num�riques
% t_fin = 50;
% 
% 
% %Conditions Initales
% Goemonts_0 = [1000, 5000, 30000, 10000];
% Patelles_0 = [100, 500, 3000, 10000];
% 
% 
% %Pour tracé de figure
% couleur = ['r' , 'g', 'b', 'm'];
% 
% 
% 
% for i=1:4
%     % On envoit � chaque fois les conditions initiales
%     [temps, solution] = solveur(Patelles_0(i), Goemonts_0(i), t_fin); 
%        
% %Goémonts    
% hold on     
% figure(1)
% plot(temps,solution(:,1),couleur(i))
% ylabel('Goemonts')
% xlabel('Temps (s)')
% legend(num2str(Goemonts_0(:)))
% title('Nombre de Goemonts en fonction du temps')
% grid on
% hold off
% 
% %Patelles
% hold on
% figure(2)
% plot(temps,solution(:,2),couleur(i))
% ylabel('Patelles')
% xlabel('Temps (s)')
% legend(num2str(Patelles_0(:)))
% title('Nombre de Patelles en fonction du temps')
% grid on
% hold off
%     
% %Plan (G,P)
% hold on
% figure(3)
% plot(solution(:,1),solution(:,2),couleur(i))
% xlabel('Goemonts')
% ylabel('Patelles')
% title('Plan (G,P)')
% grid on
% legend(sprintf('(%d, %d)',(Goemonts_0(1)),(Patelles_0(1))),...
%     sprintf('(%d, %d)',(Goemonts_0(2)),(Patelles_0(2))),...
%     sprintf('(%d, %d)',(Goemonts_0(3)),(Patelles_0(3))),...
%     sprintf('(%d, %d)',(Goemonts_0(4)),(Patelles_0(4))))
% hold off
% %     
% end

[G,P] = meshgrid(0:100:100000,0:100:100000);
y=-P*k3-k2*G*log(P)+k1*log(P);
x=G*beta*k3-k4*log(G)-k5*P*log(G);
surf(x,y)
hold on
quiver(x,y)
hold off



function [temps, solution] = solveur(Patelles_0, Goemonts_0, t_fin)

[temps,solution] = ode45(@(temps,Y)systeme(temps,Y), [0, t_fin], [Goemonts_0, Patelles_0]);%appel au solveur ode45
end


function out = systeme(temps, Y) 
global k1 k2 k3 k4 k5 beta
    out = zeros(2,1);%initialisation d'un vecteur dimensions 2*1
    out(1) = Y(1) * (k1 - k2*Y(1) - k3*Y(2));
    out(2) = Y(2) * (beta*k3*Y(1) - k4 - k5*Y(2));
end


