%%%%%%%%%%%%%%%%%%%%%%%%
% Paramï¿½tres entrï¿½es
%%% Maillage temporel et spatial
nptz=1000;
L=1000; %m = H
dt=1e-4; %s
tfinal=15;
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
npastemps=(tfinal-0)/dt;
%%%%%%%%%%%%%%%%%%%%%%%
% Initialisation
k=zeros(1,nptz);
k_bis=zeros(1,nptz);
znoeuds=zeros(1,nptz);
l=zeros(1,nptz);
zcentre=zeros(1,nptz-1);
u_l=zeros(1,nptz);
v_l=zeros(1,nptz);
K0_vect=K0*ones(1000,1);
u_t=zeros(npastemps,nptz);
v_t=zeros(npastemps,nptz);

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

%Longueur de mélange
for i=1:nptz
    l(i)=(kappa*znoeuds(i))/(1+(kappa*znoeuds(i)/lambda));
end

%determination u_l, v_l
for i=2:nptz-1
    u_l(i)=ug*(1-exp(-gamma*znoeuds(i))*cos(gamma*znoeuds(i)));
    v_l(i)=ug*exp(-gamma*znoeuds(i))*sin(gamma*znoeuds(i));
end    

% CL
[u_l,v_l]=CL(u_l,v_l,nptz,ug);

% determination k (boucle ou diff) : donnent des résultats identiques
for i=1:nptz-1
    k(i)=(l(i)^2)*sqrt(((u_l(i+1)-u_l(i))/(znoeuds(i+1)-znoeuds(i)))^2 +((v_l(i+1)-v_l(i))/(znoeuds(i+1)-znoeuds(i)))^2);
end 
k_bis=l(1:nptz-1).^2 .*sqrt( (diff(u_l)./diff(znoeuds)).^2+ (diff(v_l)./diff(znoeuds)).^2);
        

% calcul u_t et v_t

%1ere iteration
%d2u=(diff(u_l)./diff(znoeuds)).*k(1:nptz-1);
%d2u=diff(d2u)./diff(znoeuds(1:nptz-1));

%d2v=(diff(v_l)./diff(znoeuds)).*k(1:nptz-1);
%d2v=diff(d2v)./diff(znoeuds(1:nptz-1));


%Remplissage premier t
u_t(1,:)=u_l;
v_t(1,:)=v_l;


%pour le temps
for t=1:npastemps-1
    
    %1ere iteration
    if t==1
    d2u=(diff(u_l)./diff(znoeuds)).*k(1:nptz-1);
    d2u=diff(d2u)./diff(znoeuds(1:nptz-1));

    d2v=(diff(v_l)./diff(znoeuds)).*k(1:nptz-1);
    d2v=diff(d2v)./diff(znoeuds(1:nptz-1));
    
    else
    d2u=(diff(u_t(t,:))./diff(znoeuds)).*k(1:nptz-1);
    d2u=diff(d2u)./diff(znoeuds(1:nptz-1));

    d2v=(diff(v_t(t,:))./diff(znoeuds)).*k(1:nptz-1);
    d2v=diff(d2v)./diff(znoeuds(1:nptz-1));
    end
    
    u_t(t+1,2:nptz-1)=u_t(t,2:nptz-1)+dt*(d2u+f.*v_t(t,2:nptz-1));
    v_t(t+1,2:nptz-1)=v_t(t,2:nptz-1)+dt*(d2v-f.*u_t(t,2:nptz-1)+Gy);
    
    [u_t(t,:),v_t(t,:)]=CL(u_t(t,:),v_t(t,:),nptz,ug);%remplit 1 et nptz

end    



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
plot(u_l,znoeuds)
plot(v_l,znoeuds)
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
plot(k_bis,znoeuds(1:nptz-1))
plot(K0_vect,znoeuds)
ylabel('$z$','Interpreter','Latex')
xlabel('$k$','Interpreter','Latex')
legend('k','k0','k_bis')
title('Evolution des profils')
grid on
hold off

%u(v)
figure(4)
hold on
plot(u_l,v_l)
ylabel('$z$','Interpreter','Latex')
xlabel('$u$,$v$','Interpreter','Latex')
xlim([0 12])
title('Evolution des profils')
grid on
hold off

%spirale
figure(5)
hold on
plot(u_t,v_t)
xlabel('u')
ylabel('v')
title('u fonction de v')
grid on
hold off

%u_t(z),v_t(z)
figure(6)
hold on
plot(u_t,znoeuds)
plot(v_t,znoeuds)
ylabel('$z$','Interpreter','Latex')
xlabel('$u$,$v$','Interpreter','Latex')
legend('u','v')
title('Evolution des profils')
grid on
hold off



path = pwd ;   % mention your path 
myfolder = 'graphe' ;   % new folder name 
folder = mkdir([path,filesep,myfolder]) ;
path  = [path,filesep,myfolder] ;
for i = 1:6
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
