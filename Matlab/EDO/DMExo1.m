clear all
close all

x = linspace(0,1000,10000);

Kd = 0.3;
Kr = 0.41;
Do = 7.6;
Dosat = 9.1;
V=0.3;
Lo = 10.9;

tmax = log((Kr*Kd*(Lo+Do) - Kr^2*Do)/(Kd^2*Lo))/(Kr-Kd);

Dmax=D(tmax)

function [out]=D(t)
Kd = 0.3;
Kr = 0.41;
Do = 7.6;
Dosat = 9.1;
V=0.3;
Lo = 10.9;

out=(Kd*Lo)*(exp(-t*Kd)-exp(-Kr*t))/(Kr-Kd) + Do*exp(-Kr*t);
end

