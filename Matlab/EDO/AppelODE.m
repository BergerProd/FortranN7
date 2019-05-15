%Appel de l'ODE45
y0 = [1;1]; % Conditions initailes, doit �tre un vecteur colonne (;)
temps = [0 50]; % temps initial et temps final
for mu=0.1:0.1:10 %on fait varier mu de 0.1 � 10 par pas de 0.1 
[t,y]=ode45(@(t,y)vanderpol(t,y,mu),temps,y0); %appel d'ODE45, 
%formulation de fonction vanderpol anonyme pour avoir mu comme param�tre et la faire varier
%Renvoi le temps et le vecteur y,
%Le vecteur y colonne est compos� en y(1) de la fonction et en y(2) est  la
%d�riv�e

%Tra�age de la fonciton en fonction du temps
figure(1)
plot(t,y(:,1))%tra�age du vecteur y 
grid on
pause(0.2)

%Tra�age dans l'espace de phase
figure(2)
plot(y(:,1),y(:,2))
grid on
pause(0.2)
end


