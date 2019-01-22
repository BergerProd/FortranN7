set terminal latex
set output 'Untitled-gnuplottex-fig5.tex'
set size 1,1; set title "Portée en fonction de l'Angle Initial";set xrange [20:80]; set yrange [60:180];set ylabel "portée (m)" offset 2,9 ; set xlabel "$\alpha$ (degrés)"; plot "Paramétrisation_Alpha_Euler_Chute_Libre.out" using 2:3 title "Méthode Analytique" with lines, "Paramétrisation_Alpha_RK4_Chute_Libre.out" using 2:5 title "Méthode RK4" with lines
