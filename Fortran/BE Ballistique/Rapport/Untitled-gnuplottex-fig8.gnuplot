set terminal latex
set output 'Untitled-gnuplottex-fig8.tex'
set size 1,1; set title "Portée en fonction de l'Angle Initial";set xrange [20:80]; set yrange [250:450];set ylabel "portée (m)" offset 2,9 ; set xlabel "$\alpha$ (degrés)"; plot "Paramétrisation_Alpha_Euler_Propulsé.out" using 2:3 title "Méthode Euler" with lines, "Paramétrisation_Alpha_RK4_Propulsé.out" using 2:3 title "Méthode RK4" with lines
