clc
clear
close all   

k1 = 1.1;
    k2 = 1e-5;
    k3 = 1e-3;
    k4 = 0.9;
    k5 = 1e-4;
    beta =0.02;
    
%     
%     P = linspace(-10000,10000,20000);
%     G = linspace(-10000,10000,20000);
%     
%     for i=1:100:1000
%     X(i,:) = k1/k2 - k3/k2.*P + i; % G
%     
%     Y(i,:) = k1/k3 -k2/k3.*G + i; %P
%     
%     end
%     %Y2 = -k4/k5 + (beta*k3).*G/k5; %P
% %         X2 = k1/k2 - k3/k2.*P + 10; % G
% %     
% %     Y2 = k1/k3 -k2/k3.*G +10; %P
%     plot(X,Y)
    
    
    
%     //********************************************************************
% // Mod�le de Lotka-Volterra
% // Dominique Lefebvre  Octobre 2012
% // TangenteX.com
% //********************************************************************

% // param�tres initiaux des populations
a = 3;   %// taux de reproduction des proies isol�es
b = 1;   %// taux de mortalit� des proies en pr�sence de pr�dateurs 
c = 1;   %// taux de mortalit� des pr�dateurs isol�s
d = 2;   %// taux de reproduction des pr�dateurs en pr�sence de proies


% // param�tres de simulation
t0 = 0;
tmax = 20;
dt = 0.1;
x0 = 5;  %// population initiale des proies
y0 = 2;  %// population initiale des pr�dateurs

% // initialisation des vecteurs
t  = [t0:dt:tmax];
y0 = [x0;y0];  %// population initiale proies et pr�dateurs

% // r�solution du syst�me
y  = ode(y0,t0,t,LotkaVolterra);

% // trac�
subplot(2,1,1);plot2d(t,y(1,:));xtitle('Evolution des populations','Temps','Population');
subplot(2,1,1);plot2d(t,y(2,:));
subplot(2,1,2);plot2d(y(1,:),y(2,:));xtitle('Portrait de phase','Proies','Pr�dateurs');

% // syst�me diff�rentiel de Lotka-Volterra
% //y1 = population des proies, y2 = population des pr�dateurs
function [w] = LotkaVolterra(t,y)
  w(1) = a*y(1) - b*y(1)*y(2);
  w(2) = c*y(1)*y(2) - d*y(2);
end
    
    