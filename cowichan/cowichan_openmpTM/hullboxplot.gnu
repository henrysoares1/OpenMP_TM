set term png
set output "hullGCCTMOut.png"
set boxwidth 0.5
set title "Hull with openMP and GCC-TM"
set style fill solid 0.5
set xlabel ""
set ylabel "Execution time in seconds"
set grid layerdefault
set xtics ("2 Threads" 1, "4 Threads" 2, "8 Threads" 3, "16 Threads" 4, "32 Threads" 5, "64 Threads" 6, "128 Threads" 7)
set xtics rotate by -50
plot "hullOut2.txt" using (1):1 notitle with boxplot, "hullOut4.txt" using (2):1 notitle with boxplot, "hullOut8.txt" using (3):1 notitle with boxplot, "hullOut16.txt" using (4):1 notitle with boxplot, "hullOut32.txt" using (5):1 notitle with boxplot, "hullOut64.txt" using (6):1 notitle with boxplot, "hullOut128.txt" using (7):1 notitle with boxplot