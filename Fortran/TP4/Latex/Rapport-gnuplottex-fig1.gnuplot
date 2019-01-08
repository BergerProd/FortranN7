set terminal latex
set output 'Rapport-gnuplottex-fig1.tex'
set size 1,1; set title "Validation NACA23112";  plot "Airfoil_23112.out" using 1:2 title "Airfoil.com" with lines lt rgb "violet", "tp4_NACA_23112_npt_00150.out" using 2:7 title "Fortran cambrure" with lines, "tp4_NACA_23112_npt_00150.out" using 4:6 title "Fortran ye" with lines, "tp4_NACA_23112_npt_00150.out" using 3:5 title "Fortran yi" with lines


