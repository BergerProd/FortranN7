path = pwd ;   % mention your path 
myfolder = 'graphe' ;   % new folder name 
folder = mkdir([path,filesep,myfolder]) ;
path  = [path,filesep,myfolder] ;
for k = 1:15
    figure(k);
    temp=[path,filesep,'fig',num2str(k),'.png'];
    saveas(gcf,temp);
end