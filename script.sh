#!/bin/bash


echo "ceci est mon premier script" # affichage d’un message à l’écran
echo # je saute une ligne
cd ~/Documents/Informatique
# je suis sûr d’être là où je veux ...
# PARTIE 1
dir=TEST
echo "test de l’existence du dossier " $dir
# les variables sont repérées par $ quand on les utilise.
if [ -d $dir ]; then
echo "Dossier " $dir " existe"
else
echo "le dossier" $dir "n’existe pas "
mkdir $dir
# je le crée
fi
cd $dir
# voilà je suis sûr qu’il est là.
# PARTIE 2
racine="file_"
ext=".txt"
# racine d’un fichier que je peux changer à volonté
# extension d’un fichier que je peux changer à volonté
# création d’une boucle pour générer n fichiers ayant quasiment le même nom
n=5
for (( i=0;i<$n;i++)); do
fichier=$racine$i$ext
echo "fichier $fichier créé"
let j=$i*$i+$i
# création d’un nombre
echo $i $j > $fichier
# je mets deux nombres dans ce fichier
done
ls
# affichage du résultat
tar -cvf archive.tar $racine* # je créé une archive des fichiers
cat $racine* > sortie.out # je concatène tous les fichiers ensemble
# PARTIE 3
echo "recherche de 3 :"
grep 3 $racine*
# je cherche dans quel fichier se trouve le nombre 3
rm -f $racine*
# j’efface les fichiers créés ayant la racine :
# PARTIE 4
# une boucle :
echo "avez-vous mis des arguments en ligne de commande ?"


if [ -z $1 ]; then # respecter les espaces de part et d’autre de [ et ]
echo ’aucun argument lors du lancement du shell’
else
echo "liste des arguments :" $@
echo "nombre d’arguments :" $#
echo "premier argument :" $1
fi
