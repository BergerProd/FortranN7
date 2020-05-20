#Fonction pour connaitre si pair ou impair
def pair(numero):
    if numero % 2 == 0:
        #pair
        sortie = True
    else:
        sortie = False
    return sortie

a=pair(3)
print(a)
