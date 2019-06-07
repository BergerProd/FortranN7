% //********************************************************************
% // Modèle de Lotka-Volterra
% // Tracé du portait de phase
% // Dominique Lefebvre  Octobre 2012
% // TangenteX.com
% //********************************************************************
% // paramètres initiaux des populations
a = 3;   %// taux de reproduction des proies isolées
b = 1;   %// taux de mortalité des proies en présence de prédateurs 
c = 1;   %// taux de mortalité des prédateurs isolés
d = 2;   %// taux de reproduction des prédateurs en présence de proies

% // paramètres de simulation
t0 = 0;
tmax = 20;
dt = 0.01;

% // paramètres de tracé du champ
xmax = 10; dx = 0.5;
ymax = 10; dy = 0.5;

% // zone de tracé du champ de vecteurs
X = 0:dx:xmax;
Y = 0:dy:ymax;
fchamp(LotkaVolterra,0,X,Y);

%// initialisation des vecteurs
t  = [t0:dt:tmax];

%// tracé du champ de vecteurs et des orbites
xtitle("Modèle proies-prédateurs - Diagramme de phase");
while t
  [cbouton,x0,y0] = xclick();  
  if cbouton == 5 then
    break
  else
    if x0 >= 0 & x0 <= xmax & y0 >= 0 & y0 <= ymax then %//point sélectionné dans le champ ?
      [y] = ode([x0;y0],t0,t,LotkaVolterra);
      plot2d(y(1,:),y(2,:));
    end
  end 
end

% // système différentiel de Lotka-Volterra
% //y1 = population des proies, y2 = population des prédateurs

function [w] = LotkaVolterra(t,y)
 w(1) = a*y(1) - b*y(1)*y(2);
 w(2) = c*y(1)*y(2) - d*y(2);
end
