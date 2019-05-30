
clear all
close all


t=1;

for y=1:10000
    tmp = 0;
    for n=1:10000

        tmp = tmp + (2*sqrt(2)*sqrt(2)*(-y)*sin((2*n+1)*pi*y)*exp(-(2*n +1)^2 * pi^2 * y))/((2*n+1)*pi);
    
    end
    C(y)=tmp;
end

y=linspace(0,1,10000);

plot(y,C)
hold on
grid on
