#!/usr/bin/gnuplot -p -e
export DISPLAY=:0 gnuplot
set terminal png
set output "graph.png"
set xlabel "corde" ! on nomme l'axe des abscisses "abscisses"
set xrange [0,l]   ! on va tracer les abscisses entre -8 et 8
set title nom ! on va nommer le graph
plot "Airfoil_4412.out" using 1:2 title "Airfoil.com" with lines, "tp4_NACA_4412_npt_00150.out" using 2:4 title "Fortran cambrure" with lines, "tp4_NACA_4412_npt_00150.out" using 2:6 title "Fortran ye", "tp4_NACA_4412_npt_00150.out" using 2:8 title "Fortran yi"
pause 10
exit
