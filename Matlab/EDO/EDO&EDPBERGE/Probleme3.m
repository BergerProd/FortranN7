
clear all
close all
t=10;
tmp=0;
C=zeros(1,t);
nbr=9999;
y=linspace(0,1,10000);

for t=1:10
        tmp = 0;
            for n=1:nbr
        tmp = tmp + ((2*(-1)^n)*sin(n*pi.*y)*exp(-n^2*pi^2*t))/(n*pi);
          
            end
        C(t)=tmp;
    
    
end

