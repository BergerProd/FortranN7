set terminal latex
set output 'Untitled-gnuplottex-fig2.tex'
set size 1,1; set title "Validation Chute Libre RK4";set xrange [0:180]; set yrange [0:140];set ylabel "z (m)" ; set xlabel "x (m)" ; plot "BE_RK4_Chute_Libre_npt_10000.out" using 3:4 title "Méthode RK4" with lines, "BE_RK4_Chute_Libre_npt_10000.out" using 5:6 title "Méthode Analytique" with lines
