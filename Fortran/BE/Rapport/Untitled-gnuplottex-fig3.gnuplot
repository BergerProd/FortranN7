set terminal latex
set output 'Untitled-gnuplottex-fig3.tex'
set size 1,1; set title "Validation Propulsé RK4 Euler";set xrange [0:370]; set yrange [0:140];set ylabel "z (m)" ; set xlabel "x (m)"; plot "BE_RK4_Propulsé_npt_10000.out" using 3:4 title "Méthode RK4" with lines, "BE_Euler_Propulsé_npt_10000.out" using 3:4 title "Méthode Euler" with lines
