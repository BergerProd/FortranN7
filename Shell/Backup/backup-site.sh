#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Vous devez indiquer le chemin du fichier de configuration. Exemple : ./backup-site.sh /home/pi/monsite.cfg"
    exit 1
fi

### Inclusion du fichier de configuration ###
source $1

### Nom des fichiers et des répertoires ###
DATE_MONTH=`date +%Y%m`
BACKUP_DIR="$REMOTE_DIR$PROJECT/$DATE_MONTH/"

DATE_FULL=`date +%Y%m%d-%H%M%S`
BACKUP_FILENAME="$PROJECT-$DATE_FULL.tar.gz"

### Création du répertoire distant ###
ssh -p $SSH_PASS $SSH_USER@$SSH_HOST:$REMOTE_DIR$ "mkdir -p  $BACKUP_DIR"


### Création de la sauvegarde sur le serveur ###
tar -cf $BACKUP_FILENAME .tar $REMOTE_DIR

### Récupération du fichier ###
sshpass -p $SSH_PASS scp $SSH_USER@$SSH_HOST:$REMOTE_DIR$BACKUP_FILENAME "$BACKUP_DIR"

### Suppression du fichier sur le serveur ###
#sshpass -p $SSH_PASS ssh $SSH_USER@$SSH_HOST "rm $REMOTE_DIR$BACKUP_FILENAME"

### Définition du message de succès ###
read -d '' SUCCESS_MSG <<EOF
Sauvegarde effectuée avec succès.
Date : `date '+%d/%m/%Y %H:%M:%S'`
Emplacement : $BACKUP_DIR
Nom de fichier : $BACKUP_FILENAME
EOF

### Définition du message d'échec ###
read -d '' FAIL_MSG <<EOF
Sauvegarde échouée, le fichier n'a pas été trouvé dans la destination.
Date : `date '+%d/%m/%Y %H:%M:%S'`
Emplacement : $BACKUP_DIR
Nom de fichier : $BACKUP_FILENAME
EOF

### Vérification de la présence du fichier dans la destination et envoi de l'email ###
if [ -f $BACKUP_DIR$BACKUP_FILENAME ]; then
    echo "$SUCCESS_MSG" | mail -s "Sauvegarde terminée : $PROJECT" $EMAIL
else
    echo "$FAIL_MSG" | mail -s "Sauvegarde échouée : $PROJECT" $EMAIL
fi