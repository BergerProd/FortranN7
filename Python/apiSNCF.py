# -*- coding: utf-8 -*-
token_auth = '19a0622c-2005-4460-8a1a-acc2019f4136'

import keyring, os
if "19a0622c-2005-4460-8a1a-acc2019f4136" in token_auth:
    token_auth = keyring.get_password("sncf","ensae_teaching_cs,key")

import requests

##### une fonction qui sera utile pour calculer des temps

from datetime import datetime, timedelta

def convertir_en_temps(chaine) :
    ''' on convertit en date la chaine de caractères de l API'''
    return datetime.strptime(chaine.replace('T',''),'%Y%m%d%H%M%S')

def convertir_en_chaine(dt) :
    ''' on convertit en chaîne de caractères un datetime'''
    return datetime.strftime(dt, '%Y%m%dT%H%M%S')

# informations sur le trajet qu'on choisit dans le futur
# l'API ne retourne pas de résultatq très loin dans le passé
now = datetime.now()
dt = now + timedelta(14)  # dans deux semaines

date_depart = convertir_en_chaine(dt)
gare_depart = 'stop_area:OCE:SA:87686006'
gare_arrivee = 'stop_area:OCE:SA:87722025'

# ensemble des départs

paris_lyon = requests.get('https://api.sncf.com/v1/coverage/sncf/journeys?'\
                          'from={}&to={}&datetime={}'.format(gare_depart, gare_arrivee, date_depart), \
                         auth=(token_auth, '')).json()

print(date_depart)

#seyne_marseille = requests.get('https://api.navitia.io/v1/coverage/sncf/stop_points/\
#stop_point%3AOCE%3ASP%3ATrainTER-87755264/routes/route%3AOCE%3A336-TrainTER-87755009-87751008/\
#departures?from_datetime=20200114T150000'.format(...),auth=(token_auth,'')).json()

#todo definir les variables pour now, et ranger en format en json
print(paris_lyon)
