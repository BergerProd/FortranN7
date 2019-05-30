clear all
close all

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
%mode et les repr�senter dans le subplot(1,2,1).
%   (2) Determiner la fonction de repartition empirique (CDF)de la variable
%aleatoire Xemp et la repr�senter avec des barres dans le subplot(1,2,2).
%   (3) Calculer le 1er decile, la mediane et le 95eme centile et les 
%repr�senter dans le subplot(1,2,2).

scrsz = get(0,'ScreenSize');
figure('Position',[1 scrsz(4)/2 scrsz(3)/1.5 scrsz(4)/3])
figure (1)
subplot(1,2,1)% subplot(1,2) : tracé de matrice de figure 1 ligne, 2 colonnes, et 1er cadran donc fig de gauche
%Histogramme de la PDF empirique
bar(Xemp,femp,'w')
hold on
legend('empirique','min','max','mode') %à completer 
xlabel('$X$ : absence de ceinture','interpreter','latex','fontsize',14)
ylabel('$f$ : frequence empirique','interpreter','latex','fontsize',14)
axis([5 32 0 0.12])
hold off


subplot(1,2,2)
%CDF empirique
hold on
legend('empirique','1er decile','mediane','95eme centile','location','NorthWest')
xlabel('$X$ : absence de ceinture','interpreter','latex','fontsize',14)
ylabel('$CDF$ : frequence cumulee empirique','interpreter','latex','fontsize',14)
axis([5 32 0 1])
hold off

























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
 




















%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%CALCUL DE LA STATISTIQUE ET CONCLUSION%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%   (1) Caluler la fr�quence th�orique "ftheo".
%   (2) Caluler l'effectif th�orique "tk".

%Calcul de la fr�quence theorique


%Calcul de l'effectif theorique


%   (3) Y a t-il assez d'intervalles?

%   (4) Y a t-il un effectif theorique assez important dans les intervalles?
%Il faut au moins 80% des tk>5



%   (5) Evaluer l'integrale de la densite de probabilite sur son support 
%pour verifier qu'elle est bien egale a 1.

%Test sur la propriete premiere d'une densite de probabilit�:


%Test sur l'effectif theorique des observations: 



















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







%   (1) Calculer la fr�quence theorique apres regroupement "ftheo_reg"

%Calcul de la fr�quence theorique apres regroupement "ftheo_reg"
ftheo_reg=zeros(length(X_reg),1);


%Test sur la propriete premiere d'une densite de probabilit� apres 
%regroupement: Sum(ftheo_reg)=1?



%   (2) Calculer l'effectif theorique apres regroupement "tk_reg"
%Calcul de l'effectif theorique


%   (3) Y a t-il un effectif theorique assez important dans les intervalles?
%Il faut au moins 80% des tk>5





%Confrontation entre donnees empirique et proposition de loi theorique dans
%un diagramme de frequence:

%   (4) Tracer la densit� de probabilit� de la loi de Poisson regroup�e 
%"ftheo_reg" dans le subplot(1,2,2).


scrsz = get(0,'ScreenSize');
figure('Position',[1 scrsz(4)/10 scrsz(3)/1.5 scrsz(4)/3])
figure (2)
subplot(1,2,1)
hold on
bar(X_reg,femp_reg,'w')
hold off
xlabel('$X$ : absence de ceinture','interpreter','latex','fontsize',14)
ylabel('$f$ : frequence','interpreter','latex','fontsize',14)
legend('empirique','Poisson')
axis([5 32 0 0.12])


%Confrontation entre donnees empirique et proposition de loi theorique dans
%la CDF:

%   (5) Calculer la fonction de r�partition empirique des observations 
%regroupees "CDFemp_reg" et les representer avec des barres dans le 
%subplot(1,2,2).
%   (6) Calculer la fonction de repartition de la loi de Poisson regroupee
%"CDFtheo_reg" et la tracer dans le subplot(1,2,2).


subplot(1,2,2)
hold on
hold off
xlabel('$X$ : absence de ceinture','interpreter','latex','fontsize',14)
ylabel('$CDF$ : frequence cumulee','interpreter','latex','fontsize',14)
legend('empirique','Poisson','location','NorthWest')
axis([5 32 0 1])







%Calcul des ecarts entre les distributions theoriques et empiriques 
%(test du chi2)
%   (7) Realiser le test du chi2 sur les donnees regroupees

