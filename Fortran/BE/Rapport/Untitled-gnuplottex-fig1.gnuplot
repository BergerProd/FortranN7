set terminal latex
set output 'Untitled-gnuplottex-fig1.tex'
set size 1,1; set title "Validation Chute Libre Euler";set xrange [0:180]; set yrange [0:140]; plot "BE_Euler_Chute_Libre_npt_10000.out" using 3:4 title "Méthode Euler" with lines, "BE_Euler_Chute_Libre_npt_10000.out" using 5:6 title "Méthode Analytique" with lines
