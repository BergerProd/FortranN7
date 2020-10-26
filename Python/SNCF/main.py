# -*- coding: utf-8 -*-
# Pour lancer ce script, faire simplement dans un terminal en se mettant au bon endroit, 
#python3 main.py
# attention à bien changer dns pour se mettre en Google
import os
import json
from pprint import pprint
#import panda

#Lance le script shell requete.sh qui va creer en sortie le fichier.json
os.system("./requete.sh")

#manipulation du fichier json

#Ouverture fichier et copie dans variable locale
with open("seyne.json", "r") as jsonfile: #lecture fichier Json
    datas = json.load(jsonfile) #stockage dans variable 

#key c'est les grosses colonnes quoi, et values les differentes valeures
#for key, value in datas.items():
#    print(key)

#for key, value in datas.items():
#    print(key)

print(datas["departures"])

#pagination pas interessant
#links route et type de route
#disruptions pas interessant
#notes pas interessant
#feed_publishers pas interesssant
#departures
#context date actuelle de maj
#exceptions pas interessant
# Fermeture fichier
jsonfile.close()
