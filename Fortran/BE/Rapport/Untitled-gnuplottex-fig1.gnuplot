set terminal latex
set output 'Untitled-gnuplottex-fig1.tex'
set size 1,1; set title "Validation Chute Libre Euler";  plot "BE_Euler_Chute_Libre_npt_10000.out" using 3:4 title "Méthode Euler" with lines lt rgb "violet", "BE_Euler_Chute_Libre_npt_10000.out" using 5:6 title "Méthode Analytique" with lines
