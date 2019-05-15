set terminal latex
set output 'Untitled-gnuplottex-fig9.tex'
set size 1,1; set title "Temps en fonction l'Angle Initial";set xrange [20:80]; set yrange [4:10];set ylabel "t(s)" ; set xlabel "$\alpha$ (degrés)"; plot "Paramétrisation_Alpha_Euler_Propulsé.out" using 2:4 title "Méthode Euler" with lines, "Paramétrisation_Alpha_RK4_Propulsé.out" using 2:4 title "Méthode RK4" with lines
