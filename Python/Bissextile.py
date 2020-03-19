# -*- coding: utf-8 -*-
#Test Annee bissextile

#Annee bissextile si multiple de 4 mais
#Si multiple de 4, on regarde si multiple de 100
#Si multiple de 100, on regarde si multiple de 400
# Si multiple de 400,100 et 4 bissextile
# Sinon pas bissextile
#Si juste multiple de 100 et de 4 alors bissextile

annee = input("Saisissez une annee : ")
annee = int(annee)

if annee%4 ==0 :
    if annee%100 :
        if annee%400 :
            bissextile = True
        else :
            bissextile = False
        bissextile =True
    else : 
        bissextile =False        
else :
    bissextile = False
    

print("Annee",annee,"est",bissextile)            
        
        
        