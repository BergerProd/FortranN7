%%%%%%%%%%%%%%%%%%%%%%%%
% Paramï¿½tres entrï¿½es
%%% Maillage temporel et spatial
nptz=1000;
L=1000; %m = H
dt=1e-6; %s
%%% Conditions Initiales
z0=1; %m
L=L-z0;
ug=10; %m/s
%%% Conditions Limites : cf fonction
%%% Parametres
Gy=9.81; %m/s^2
rho=1;
K0=3;
kappa=0.41;
f=1e-4; %s
lambda = 2.7*1e-4*ug/f;
gamma=sqrt(f/(2*K0));
%%%%%%%%%%%%%%%%%%%%%%%
% Initialisation
k=zeros(1,nptz);
k_bis=zeros(1,nptz);
znoeuds=zeros(1,nptz);
l=zeros(1,nptz);
zcentre=zeros(1,nptz-1);
u_l_z=zeros(1,nptz);
v_l_z=zeros(1,nptz);

%% MAILLAGE
%Pas dx et dy
dz = L/real(nptz-1);

%initialisation des noeuds
znoeuds(1) = 0;

%Noeuds des mailles
for i=2:nptz
    znoeuds(i) = znoeuds(1)+dz*(i-1);
end

%Centre des Volumes
%npt-1 sur les 2
zcentre(1)= znoeuds(1)+dz/2;
for i=2:nptz-1
    zcentre(i) = zcentre(1)+ dz*(i-1);
end    

%% CL


%Longueur de mélange
for i=1:nptz
    l(i)=(kappa*znoeuds(i))/(1+(kappa*znoeuds(i)/lambda));
end

for i=2:nptz-1
    u_l_z(i)=ug*(1-exp(-gamma*znoeuds(i))*cos(gamma*znoeuds(i)));
    v_l_z(i)=ug*exp(-gamma*znoeuds(i))*sin(gamma*znoeuds(i));
end    

[u_l_z,v_l_z]=CL(u_l_z,v_l_z,nptz,ug);

for i=1:nptz-1
    k(i)=(l(i)^2)*sqrt(((u_l_z(i+1)-u_l_z(i))/(znoeuds(i+1)-znoeuds(i)))^2 +((v_l_z(i+1)-v_l_z(i))/(znoeuds(i+1)-znoeuds(i)))^2);
end 

k_bis=l(1:nptz-1).^2 .*sqrt( (diff(u_l_z)./diff(znoeuds)).^2+ (diff(v_l_z)./diff(znoeuds)).^2);
K0_vect=3*ones(1000,1);
        

A=diff(u_l_z)./diff(znoeuds);
A=A.*k(1:nptz-1);
A=diff(A)./diff(znoeuds(1:nptz-1));







%% Sortie graphique

% Longueur Mélange
figure(1)
hold on
plot(l,znoeuds)
ylabel('$z$','Interpreter','Latex')
xlabel('$l$','Interpreter','Latex')
title('Longueur de mélange')
grid on
hold off

%u(z),v(z)
figure(2)
hold on
plot(u_l_z,znoeuds)
plot(v_l_z,znoeuds)
ylabel('$z$','Interpreter','Latex')
xlabel('$u$,$v$','Interpreter','Latex')
xlim([-2 inf])
legend('u','v')
title('Evolution des profils')
grid on
hold off

%k
figure(3)
hold on
plot(k,znoeuds)
xlim([0 15])
plot(K0_vect,znoeuds)
ylabel('$z$','Interpreter','Latex')
xlabel('$k$','Interpreter','Latex')
legend('k','k0')
title('Evolution des profils')
grid on
hold off

%u(z),v(z)
figure(4)
hold on
plot(u_l_z,v_l_z)
ylabel('$z$','Interpreter','Latex')
xlabel('$u$,$v$','Interpreter','Latex')
xlim([0 12])
title('Evolution des profils')
grid on
hold off


path = pwd ;   % mention your path 
myfolder = 'graphe' ;   % new folder name 
folder = mkdir([path,filesep,myfolder]) ;
path  = [path,filesep,myfolder] ;
for i = 1:4
    figure(i);
    temp=[path,filesep,'fig',num2str(i),'.png'];
    saveas(gcf,temp);
end

%function [u,v] = euler(u,v,dt)

%Conditions limites
function [u,v] = CL(u,v,nptz,ug)
u(1)=-ug;
u(nptz)=0;
v(1)=0;
v(nptz)=0;
end
