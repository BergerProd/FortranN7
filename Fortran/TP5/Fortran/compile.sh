#!/bin/bash
echo "je compile le programme $1.f90 "
gfortran -Wsurprising  -ffree-line-length-none -O3 -Wall -g  $1.f90 -o $1.bin
echo "compilation effectuée"
exit
