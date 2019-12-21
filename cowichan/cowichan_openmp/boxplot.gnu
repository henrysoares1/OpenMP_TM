set term png
set output "out.png"
set boxwidth 0.5
set style fill solid 0.5
set xlabel ""
set ylabel "Tempo de execução em segundos"
set grid layerdefault
set xtics ("4 Threads" 1, "20 Threads" 2)
set xtics rotate by -50
plot "out.txt" using (1):1 notitle with boxplot, "out.txt" using (2):2 notitle with boxplot