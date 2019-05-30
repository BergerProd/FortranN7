S(1,1).nom = 'Caro-Line';
S(1,1).age = 33;
S(1,1).fonction = 'Modératrice MATLAB';

S(2,1).nom = 'Dut';
S(2,1).age = 30;
S(2,1).fonction = 'Responsable MATLAB';

S(3,1).nom = 'tug83';
S(3,1).age = 27;
S(3,1).fonction = 'Modérateur MATLAB';

S(4,1).nom = 'rostomus';
S(4,1).age = 24;
S(4,1).fonction = 'Modérateur MATLAB';

clear all
close all

C1 = 0
C2 =0
C3=0
C4=0
tmp = 0;
t=1;

for x=1:10000
    for n=1:10000

    tmp = tmp -x*sqrt(2)*(1-cos(n*pi))*sqrt(2).*sin(n*pi*x).*exp(-n^2 * pi^2 *t)/(n*pi);
    
    end
    C1(x)=tmp;
end

tmp = 0;
t=2;

for x=1:10000
    for n=1:10000

    tmp = tmp -x*sqrt(2)*(1-cos(n*pi))*sqrt(2).*sin(n*pi*x).*exp(-n^2 * pi^2 *t)/(n*pi);
    
    end
    C2(x)=tmp;
end

tmp = 0;
t=3;

for x=1:10000
    for n=1:10000

    tmp = tmp -x*sqrt(2)*(1-cos(n*pi))*sqrt(2).*sin(n*pi*x).*exp(-n^2 * pi^2 *t)/(n*pi);
    
    end
    C3(x)=tmp;
end

tmp = 0;
t=4;

for x=1:10000
    for n=1:10000

    tmp = tmp -x*sqrt(2)*(1-cos(n*pi))*sqrt(2).*sin(n*pi*x).*exp(-n^2 * pi^2 *t)/(n*pi);
    
    end
    C4(x)=tmp;
end



x=linspace(0,1,10000);

figure(1)
plot(x,C1,x,C2,x,C3,x,C4)

hold on
axis [-1 10 -1 1]
grid on
hold off
