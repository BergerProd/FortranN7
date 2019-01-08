#!/bin/bash
echo "je compile le programme $1.f90 "
gfortran $1.f90 -o $1.bin
echo "compilation effectuée"
exit
