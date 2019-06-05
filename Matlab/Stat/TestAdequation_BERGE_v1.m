clear all
close all

%% INITIALISATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%----DONNEES EMPIRIQUES----%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Xemp:"absence de ceinture de s�curit�"
Xemp=(7:30)'; %X empirique Absence de ceubtyre de sécurité
%Femp(i):nombre de jours o� Xemp(i) a �t� observ� normalis� par 300 jours 
%d'observation au total
femp=[1 2 2 7 6 14 14 20 27 22 23 27 30 26 18 15 17 12 6 3 3 2 2 1]'/300; %Fréquence empirique
%Nombre de valeurs que prend la v.a.
n=length(Xemp);













%%%%%%%%%%%%%%%%%%%%%%%%%
%STATISTIQUE DESCRIPTIVE%
%%%%%%%%%%%%%%%%%%%%%%%%%

%   (1) D�terminer les valeurs extremales de la variable al�atoire et son
%mode et les repr�senter dans le subplot(1,2,1). %!Fait!%
%   (2) Determiner la fonction de repartition empirique (CDF)de la variable
%aleatoire Xemp et la repr�senter avec des barres dans le subplot(1,2,2). %!Fait!%
%   (3) Calculer le 1er decile, la mediane et le 95eme centile et les 
%repr�senter dans le subplot(1,2,2).%!Fait!%

[maxX,maxXi]=max(Xemp); %on récupère le max des fréquence, ainsi que l'indice de ce max
[minX,minXi]=min(Xemp);
%Pour le mode c'est la valeur la plus fréquente => max de f
[modef,modefi]=max(femp);



% calcul de la CDF empirique
CDF=zeros(1,n);
CDF(1)=femp(1);
for i=2:n
    CDF(i)=CDF(i-1)+femp(i);
end

%find renvoi direct l'indice
di1=find(CDF>0.1,1);%1er décile
medi=find(CDF>0.5,1);%mediane
c95i=find(CDF>0.95,1);%95 centile




























%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%CHOIX DE LA THEORIE ET CALCUL DES PARAMETRES%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%---------------LOI DE POISSON---------------%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Loi de Poisson : p(x;lambda)=lambda^x exp(-lambda)/factorial(x)
%la loi de Poisson a un parametre: lambda (note l dans la routine)
%E(x)=lambda


%   (1) Caluler le param�tre lambda (not� l dans la routine) de la loi de 
%Poisson.
 

mtmp=0;
for i=1:n
    mtmp = mtmp+ Xemp(i)*femp(i)/(sum(femp));
end

l=sum(Xemp.*femp);%autre calcul de la moyenne = lambda %définition du lambda

















%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%CALCUL DE LA STATISTIQUE ET CONCLUSION%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%   (1) Caluler la fr�quence th�orique "ftheo".
%   (2) Caluler l'effectif th�orique "tk".

%Calcul de la fr�quence theorique

ftheo=zeros(1,n);
for i=1:n
    ftheo(i)=l^Xemp(i) * exp(-l)/factorial(Xemp(i));
end

%Calcul de l'effectif theorique
tk=zeros(1,n);
tk=300.*ftheo;


%   (3) Y a t-il assez d'intervalles?

% Il doit y avoir au moins 8 intervalles, ici il y en a 24 donc c'est bon

nb_intvl = length(tk);

if nb_intvl < 8
    disp("il n'y a pas assez d'intervalles pour utiliser la loi du khi²")
end

%   (4) Y a t-il un effectif theorique assez important dans les intervalles?
%Il faut au moins 80% des tk>5

tk_5=find(tk>=5);
int_80=length(tk_5)/length(tk);

if int_80<0.8
    disp("on est inferieur à 80%, il n'y a pas un effectif théorique assez important")
else
    disp("il faut regrouper les intervalles")
end


%   (5) Evaluer l'integrale de la densite de probabilite sur son support 
%pour verifier qu'elle est bien egale a 1.


%Test sur la propriete premiere d'une densite de probabilit�:

int_ddp=sum(ftheo);

if int_ddp<1
    disp("l'intégrale de la ddp est inferieure à 1,il manque donc des valeurs")
end

%Test sur l'effectif theorique des observations: 

tk_tot = sum(tk);

if tk_tot<300
    disp("l'effectif théorique est inferieur à 300")
end

















%Les valeurs extremes de la variable aleatoire n'ont pas ete prises en
%compte et par cons�quent la loi theorique ne repond pas a la propriete
%premiere qui lui est attribuee, a savoir le fait que son integrale sur son
%support soit egale a 1. Pour remedier a cela, il faut operer a un
%regroupement des intervalles et faire en sorte de considerer tout ce qui
%se passe pour une valeur de la variable aleatoire plus petite que la
%valeur minimale observee et plus grande que la valeur maximale observee.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%------REGROUPEMENT DES INTERVALLES-------%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%X_reg represente le vecteur des valeurs regroupees que peut prendre la
%variable aleatoire. Le minimum 9 sous-entend toutes les valeurs de X<=9 et
%le maximum 27 sous entend toutes les valeurs de X>=27. La frequence
%empirique regroupee femp_reg et rearangee en consequence.
X_reg=(9:27)'; 
femp_reg=[5 7 6 14 14 20 27 22 23 27 30 26 18 15 17 12 6 3 8]'/300;
n_theo_reg = 300;


%   (1) Calculer la fr�quence theorique apres regroupement "ftheo_reg"


%Calcul de la fr�quence theorique apres regroupement "ftheo_reg"
ftheo_reg=zeros(length(X_reg),1);%initialisation

%Boucle pour remplir la 1ère valeur
for i=0:9 
    ftheo_reg(1)=l^i * exp(-l)/factorial(i)+ftheo_reg(1);
end

%Boucle pour remplir les valeurs du milieux
for i=2:19-1
   ftheo_reg(i)=l^X_reg(i) * exp(-l)/factorial(X_reg(i));
end    

%Pour remplir la 19ème valeur on prend la ddp=1 moins les autres valeurs
ftheo_reg(19) = 1-sum(ftheo_reg);
%Test sur la propriete premiere d'une densite de probabilit� apres 
%regroupement: Sum(ftheo_reg)=1?

ddp_reg = sum(ftheo_reg);
if int_ddp<1
    disp("l'intégrale de la ddp du regroupement est inferieure à 1")
end


%   (2) Calculer l'effectif theorique apres regroupement "tk_reg"
%Calcul de l'effectif theorique

tk_reg = 300.*ftheo_reg;
n_reg =sum(tk_reg);


%   (3) Y a t-il un effectif theorique assez important dans les intervalles?
%Il faut au moins 80% des tk>5

tk_5_reg=find(tk_reg>=5);
inf_5_reg=length(tk_5_reg)/length(tk_reg);

if inf_5_reg<0.8
    disp("on est inferieur à 80%, il n'y a pas un effectif théorique assez important")
else
    disp("On est superieur à 80% dans le regroupé")
end



%Confrontation entre donnees empirique et proposition de loi theorique dans
%un diagramme de frequence:

%   (4) Tracer la densit� de probabilit� de la loi de Poisson regroup�e 
%"ftheo_reg" dans le subplot(1,2,1).

%Cf figure 3

%Confrontation entre donnees empirique et proposition de loi theorique dans
%la CDF:

%   (5) Calculer la fonction de r�partition empirique des observations 
%regroupees "CDFemp_reg" et les representer avec des barres dans le 
%subplot(1,2,2).

CDFemp_reg=zeros(length(X_reg),1);
CDFemp_reg(1)=1/n_theo_reg;
for i=2:19
    CDFemp_reg(i)=sum(femp_reg(1:i));
end



%   (6) Calculer la fonction de repartition de la loi de Poisson regroupee
%"CDFtheo_reg" et la tracer dans le subplot(1,2,2).

CDFtheo_reg=zeros(length(X_reg),1);
CDFtheo_reg(1)=1/n_theo_reg;
for i=2:19
    CDFtheo_reg(i)=sum(ftheo_reg(1:i));
end







%Calcul des ecarts entre les distributions theoriques et empiriques 
%(test du chi2)
%   (7) Realiser le test du chi2 sur les donnees regroupees
chi2 = sum((femp_reg.* n_theo_reg - tk_reg).^2 ./ tk_reg ) 

ddl = 19-1-1; %degrés de libertés k-r-1

% On trouve un chi2 = 8.3295 comparé au chi2 à 0.05% qui est dans la table
% de 27.857, on peut alors approuver l'hypothese H0 avec un risque de se
% tromper de 5%
% On peut monter jusqu'à la valeur de 95% sans risquer de se tromper


%% FIGURES

% Figure 1
scrsz = get(0,'ScreenSize');
figure('Position',[1 scrsz(4)/2 scrsz(3)/1.5 scrsz(4)/3])
figure (1)
subplot(1,2,1)% subplot(1,2) : tracé de matrice de figure 1 ligne, 2 colonnes, et 1er cadran donc fig de gauche
%Histogramme de la PDF empirique
bar(Xemp,femp,'w')
hold on
plot(minX,femp(minXi),'x',maxX,femp(minXi),'o',Xemp(modefi),modef,'d') %plot des max et min et mode
plot(Xemp,ftheo,'r')
legend('empirique','min','max','mode','théorique') 
xlabel('$X$ : absence de ceinture','interpreter','latex','fontsize',14)
ylabel('$f$ : frequence empirique','interpreter','latex','fontsize',14)
axis([5 32 0 0.12])
hold off

%Figure 2
subplot(1,2,2)

bar(Xemp,CDF)%CDF empirique en barres
hold on
plot(Xemp(di1),CDF(di1),'o',Xemp(medi),CDF(medi),'d',Xemp(c95i),CDF(c95i),'x')
legend('empirique','1er decile','mediane','95eme centile','location','NorthWest')
xlabel('$X$ : absence de ceinture','interpreter','latex','fontsize',14)
ylabel('$CDF$ : frequence cumulee empirique','interpreter','latex','fontsize',14)
axis([5 32 0 1])
hold off


%Figure 3

scrsz = get(0,'ScreenSize');
figure('Position',[1 scrsz(4)/10 scrsz(3)/1.5 scrsz(4)/3])
figure (2)
subplot(1,2,1)
hold on
bar(X_reg,femp_reg,'w')
plot(X_reg,ftheo_reg)
hold off
xlabel('$X$ : absence de ceinture','interpreter','latex','fontsize',14)
ylabel('$f$ : frequence','interpreter','latex','fontsize',14)
legend('empirique','Poisson')
axis([5 32 0 0.12])

%Figure 4
subplot(1,2,2)
hold on
bar(X_reg,CDFemp_reg,'w')
plot(X_reg,CDFtheo_reg,'b')
hold off
xlabel('$X$ : absence de ceinture','interpreter','latex','fontsize',14)
ylabel('$CDF$ : frequence cumulee','interpreter','latex','fontsize',14)
legend('empirique','Poisson','location','NorthWest')
axis([5 32 0 1])


