Le coeur du programme se situe dans le dossier Fortran.
Ce dossier Fortran contient :
- un fichier pour le module : module_reacteur_chimique.f90, 
- un fichier pour les subroutines : subroutine.f90
- le programme en lui m�me : main.f90
- le Makefile
Afin d'effectuer la compilation, il faut se placer dans ce dossier et via le terminal faire un make clean; make.

Le resultat de cette compilation g�nerera un fichier binaire qui apparaitra dans le dossier RUN.
Il suffira de le lancer avec la commande terminal ./main.bin
Le fichier parametres.in est le fichier des donn�es d'entr�e
Ce programme g�n�re en sortie sol_xxxx.vts et sol.pvd qui sont les fichiers de sortie au format ParaView