# -*- coding: utf-8 -*-

#Programme ZCasino tuto https://openclassrooms.com/fr/courses/235344-apprenez-a-programmer-en-python/231735-tp-tous-au-zcasi
from random import randrange
from math import ceil

#Fonction pour connaitre si pair ou impair
def pair(numero):
    if numero % 2 == 0:
        #pair
        sortie = True
    else:
        sortie = False
    return sortie


#Input
numero_mise = input("Sur quel numéro voulez-vous miser  ? : ")
somme_misee = input("Quelle somme voulez-vous miser ? : ")
#Gestion Erreur input
try :
    numero_mise=int(numero_mise)
    somme_misee=int(somme_misee)
    assert numero_mise < 50 and numero_mise >= 0
except : 
    print("Attention, le numero saisi doit être un entier compris entre 0 et 49")    

# Pair ou Impair
Pair_mise=pair(numero_mise)
#Generation numero gagnant
numero_gagnant = randrange(50)
Pair_gagnant=pair(numero_gagnant)

if numero_gagnant == numero_gagnant :
    somme_gagnee = somme_misee * 3
    print("Vous etes très chanceux, numero mise = ", numero_mise, "numero gagnant = ", numero_gagnant,"somme misee =", somme_misee, "somme gagnee nette ", somme_gagnee-somme_misee)
elif Pair_gagnant and Pair_mise : 
    somme_gagnee = 0.5 * somme_misee
    print("Vous etes simplement chanceux, numero mise = ", numero_mise, "numero gagnant = ", numero_gagnant, "somme misee =", somme_misee, "somme gagnee nette ", somme_gagnee-somme_misee)

else :
    print("zeroooooooo")
