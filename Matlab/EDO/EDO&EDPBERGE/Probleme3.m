
clear all
close all


nbr=9999;
a=linspace(0,1,10000);
y=1;

for t=0:10
        tmp = 0;
            for n=1:nbr
        tmp = tmp + ((2*(-1)^n)*sin(n*pi*y)*exp(-n^2*pi^2*t))/(n*pi);
        C(t+1)=tmp;   
            end
       
    
    
end

