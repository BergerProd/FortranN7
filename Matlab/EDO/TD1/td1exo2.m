%Opérations élémentaires et scripts : exo2

clear all
A=[2 -1 0
    -1 2 -1
    0 -1 2];
[V,D]=eig(A);
B=inv(A);
err=norm(B*A-eye(3))/3^0.5;

N=10;
U=2*ones(N,1);
V=-ones(N-1,1);
A10=diag(U,0)+diag(V,-1)+diag(V,1);
[V10,D10]=eig(A10);
VP10=diag(D10);

N=50;
U=2*ones(N,1);
V=-ones(N-1,1);
A50=diag(U,0)+diag(V,-1)+diag(V,1);
[V50,D50]=eig(A50);
VP50=diag(D50);

N=100;
U=2*ones(N,1);
V=-ones(N-1,1);
A100=diag(U,0)+diag(V,-1)+diag(V,1);
[V100,D100]=eig(A100);
VP100=diag(D100);

I10=linspace(0,1,11);I10=I10(2:end);
I50=linspace(0,1,51);I50=I50(2:end);
I100=linspace(0,1,101);I100=I100(2:end);

close all
plot(I10,VP10,'-o',I50,VP50,'-+',I100,VP100,'-s')
xlabel('i/N')
ylabel('eigenvalue')
legend('N=10','N=50','N=100','Location','NorthWest')

N=1000;
tic
A=zeros(N);
for i=1:N
    A(i,i)=2;
end
for i=2:N
    A(i,i-1)=-1;
end
for i=1:N-1
    A(i,i+1)=-1;
end
toc
tic
A=2*diag(ones(N,1),0)-diag(ones(N-1,1),-1)-diag(ones(N-1,1),1);
toc
