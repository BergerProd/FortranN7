function [out] = vanderpol(t,y,mu)
%Oscillation de VanderPol
x1=y(2);
x2=mu*(1-y(1)^2)*y(2)-y(1);
out=[x1;x2];
end

