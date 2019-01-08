#!/bin/bash

if [ -z $1 ] ; then
	echo "Usage : $0 src_file "
	echo "      without the .f90"
	exit
fi
echo "compilation du fichier $1.f90"
file_out=$1'.bin'
gfortran $1'.f90' -o $file_out
# mv $file_out ../.
echo "compilation effectué"
exit 0
