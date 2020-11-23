%-------------------------------
% Parametres Entree
%-------------------------------
%%% Maillage temporel et spatial
nptz=200;
L=1000; %m = H
dt=0.2; %s
tfinal=3600; %s
%%% Conditions Initiales
z0=1; %debut du domaine : m
L=L-z0;
ug=10; %vitesse : m/s
%%% Conditions Limites : cf fonction a la fin
%%% Parametres
rho=1; %masse volumique air supposee constante
K0=3; %
kappa=0.41; %kappa du modele de longueur de melange
f=1e-4; %s
Gy=rho*ug*f;
lambda = 2.7*1e-4*ug/f;
gamma=sqrt(f/(2*K0)); %parametre
npastemps=(tfinal-0)/dt; %definition du pas de temps
%-----------------------
% Initialisation des vecteurs
%-------------------------------
k=zeros(1,nptz);
k_bis=zeros(1,nptz);
znoeuds=zeros(1,nptz);
l=zeros(1,nptz);
u_l=zeros(1,nptz);
v_l=zeros(1,nptz);
K0_vect=K0*ones(nptz,1);
u_t=zeros(npastemps,nptz);
v_t=zeros(npastemps,nptz);

%-------------------------------
%% MAILLAGE
%Pas dz
dz = L/real(nptz-1);

%initialisation des noeuds
znoeuds(1) = 0;

%Noeuds des mailles
for i=2:nptz
    znoeuds(i) = znoeuds(1)+dz*(i-1);
end
%-------------------------------
%Longueur de melange
for i=1:nptz
    l(i)=(kappa*znoeuds(i))/(1+(kappa*znoeuds(i)/lambda));
end
%-------------------------------
%determination u_l, v_l
for i=2:nptz-1
    u_l(i)=ug*(1-exp(-gamma*znoeuds(i))*cos(gamma*znoeuds(i)));
    v_l(i)=ug*exp(-gamma*znoeuds(i))*sin(gamma*znoeuds(i));
end

% appel a fonction CL
[u_l,v_l]=CL(u_l,v_l,nptz,ug);

%-------------------------------
% determination k (boucle ou diff) : donnent des resultats identiques
for i=1:nptz-1
    k(i)=(l(i)^2)*sqrt(((u_l(i+1)-u_l(i))/(znoeuds(i+1)-znoeuds(i)))^2 +((v_l(i+1)-v_l(i))/(znoeuds(i+1)-znoeuds(i)))^2);
end
k_bis=l(1:nptz-1).^2 .*sqrt( (diff(u_l)./diff(znoeuds)).^2+ (diff(v_l)./diff(znoeuds)).^2);

%diff est une fonction integrée a matlab qui pour un vecteur u renvoi la valeur de la différence de u(i+1)-u(i), elle se substitue donc a la notation lourde
% diff(u) pour un vecteur de taille n renvoit une taille n-1, donc adaptation pour écrire dans le vecteur k_bis qui devra pour tracer être de la même taille que t

%-------------------------------
%Critere de convergence
%definition dtbis, dt maximum pour convergence
dtbis=(dz^2)/(max(k));
%Si dt saisi trop important, message d'erreur
if (dt>dtbis)
    disp('dt saisi = ',dt,'dt requis',dtbis);
    error('dt > dtbis');
end
%-------------------------------
% calcul u_t et v_t
%Remplissage premier temps qui seront donc les conditions initiales avec les vecteurs calcules precedemment u_l et v_l
u_t(1,:)=u_l;
v_t(1,:)=v_l;

for t=1:npastemps-1 %boucle en temps

    %1ere iteration
    if t==1
        %vecteurs d2u qui servent a calculer la derivee seconde
        %adaptation des tailles fonctions des pas de temps a parcourir puisque diff(u) renvoi un vecteur de taille n-1
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
    %remplissage du pdt t+1 avec schema euler
    u_t(t+1,2:nptz-1)=u_t(t,2:nptz-1)+dt*(d2u+f.*v_t(t,2:nptz-1));
    v_t(t+1,2:nptz-1)=v_t(t,2:nptz-1)+dt*(d2v-f.*u_t(t,2:nptz-1)+Gy);

    [u_t(t,:),v_t(t,:)]=CL(u_t(t,:),v_t(t,:),nptz,ug);%remplit 1 et nptz

end
%-------------------------------
%% Sortie graphique
%-------------------------------
% Longueur Melange
figure(1)
hold on
plot(l,znoeuds)
ylabel('$z$','Interpreter','Latex')
xlabel('$l$','Interpreter','Latex')
title('Longueur de melange')
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
title('Solutions stationnaires de u et v')
grid on
hold off

%k
figure(3)
hold on
plot(k,znoeuds)
xlim([0 15])
plot(K0_vect,znoeuds)
plot(k_bis,znoeuds(1:nptz-1))
ylabel('$z$','Interpreter','Latex')
xlabel('$k$','Interpreter','Latex')
legend('k','k0','k_{bis} (methode diff)')
title('Evolution des profils de K')
grid on
hold off

%u(v)
figure(4)
hold on
plot(u_l,v_l)
xlabel('$u$,$v$','Interpreter','Latex')
xlim([0 12])
title('Solution stationnaire de u et v dans le plan u,v')
grid on
hold off

%spirale
figure(5)
hold on
plot(u_t,v_t)
xlabel('u')
ylabel('v')
title('Solutions transitoire de u et v dans le plan u,v')
grid on
hold off

%unable to trace fig(6) too much info, maybe down nptz
%u_t(z),v_t(z)
figure(6)
hold on
plot(u_t,znoeuds)
plot(v_t,znoeuds)
ylabel('$z$','Interpreter','Latex')
xlabel('$u$,$v$','Interpreter','Latex')
legend('u','v')
title('Evolution des profils transitoire pour differents temps')
grid on
hold off

%plot differents pas de temps
figure(7)
hold on
plot(u_l,znoeuds,'m')
plot(v_l,znoeuds,'k')
for i=1:2000:npastemps
    plot(u_t(i,:),znoeuds,'g')
    plot(v_t(i,:),znoeuds,'r')
end
ylabel('$z$','Interpreter','Latex')
xlabel('$u$,$v$','Interpreter','Latex')
legend('u','v','u(t)','v(t)')
title('solutions stationnaire et regime transitoire pour differents temps')
grid on
hold off
%-------------------------------
%Sauvegarde des figures
%-------------------------------
path = pwd ;   % mention your path
myfolder = 'graphe' ;   % new folder name
folder = mkdir([path,filesep,myfolder]) ;
path  = [path,filesep,myfolder] ;
for i = 1:7
    figure(i);
    temp=[path,filesep,'fig',num2str(i),'.png'];
    saveas(gcf,temp);
end

%Conditions limites
function [u,v] = CL(u,v,nptz,ug)
u(1)=0;
u(nptz)=ug;
v(1)=0;
v(nptz)=0;

% u(1)=-ug;
% u(nptz)=0;
% v(1)=0;
% v(nptz)=0;
end
