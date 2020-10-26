#/usr/bin/sudo
#Mettre à jour fortls : pip install --upgrade fortran-language-server
echo "upgrade fortls, but 1st, let's start with pip"
echo 'upgrade pip'
python3 -m pip install --upgrade pip
echo 'upgrade fortls'
python3 -m pip install --upgrade fortran-language-server
echo "it's done"