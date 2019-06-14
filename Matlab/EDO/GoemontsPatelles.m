% eablonet
% date : 2019

clc
clear
close all


%% parameters
t_end = 40;
G0 = [5000, 80000, 100000, 20000];
P0 = [10, 200, 800, 1000];

%% solve
fig1 =figure(1);
fig1.Name = 'Evolution des proies et de prédateurs au cours du temps';

ax1 = subplot(2,1,1);
ylabel('Proies')
grid on

ax2 = subplot(2,1,2);
grid on
ylabel('Prédateurs')
xlabel('time (s)')

fig2 = figure(2);
fig2.Name = 'Proie - Prédateur : point d''inflection';
ax3 = axes();
grid on

colors = ['b' , 'r', 'g', 'k'];
marker = ['o', 's', 'v' ,'d'];

for i = 1:length(G0)
    [t, sol] = solver(P0(i), G0(i), t_end);

    hold(ax1, 'on')
    plot(t, sol(:,1), 'color', colors(i), 'Parent', ax1)
    hold(ax1, 'off')
    
    hold(ax2, 'on')
    plot(t, sol(:,2), 'color', colors(i), 'Parent', ax2)
    hold(ax2, 'off')
    
     
    hold(ax3, 'on')
    plt = plot(sol(:,2), sol(:,1), '-', 'Parent', ax3, 'DisplayName',num2str(G0));
    plot(P0(i), G0(i), 'r', 'marker', marker(i), 'markerfacecolor', 'none')
    hold(ax3, 'off')
end

legend(ax1,...
    sprintf('(%d, %d)',G0(1),P0(1)),...
    sprintf('(%d, %d)',G0(2),P0(2)),...
    sprintf('(%d, %d)',G0(3),P0(3)),...
    sprintf('(%d, %d)',G0(4),P0(4))...
);

legend(ax2,...
    sprintf('(%d, %d)',G0(1),P0(1)),...
    sprintf('(%d, %d)',G0(2),P0(2)),...
    sprintf('(%d, %d)',G0(3),P0(3)),...
    sprintf('(%d, %d)',G0(4),P0(4))...
);

%% functions
function [t, sol] = solver(P0, G0, t_end)
    k1 = 1.1;
    k2 = 1e-5;
    k3 = 1e-3;
    k4 = .9;
    k5 = 1e-4;
    beta = .02;
 
    [t,sol] = ode45(@(t,Y) sys(t,Y, k1, k2, k3, k4, k5, beta), [0, t_end], [G0, P0]);
end


function o1 = sys(t, Y, k1, k2, k3, k4, k5, beta)
    o1 = zeros(2,1);
    o1(1) = Y(1) * (k1 - k2*Y(1) - k3*Y(2));
    o1(2) = Y(2) * (beta*k3*Y(1) - k4 - k5*Y(2));
end