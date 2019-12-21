cowichan_openmp:	cowichan.o
	g++ -Wall -m64 -fopenmp -D LIN64 -O2 -o cowichan_openmp *.cpp cowichan.o

cowichan.o: ../cowichan/cowichan.cpp
	g++ -Wall -m64 -fopenmp -D LIN64 -O2 -c ../cowichan/cowichan.cpp

clean:	
	rm *.o *.txt *.png

run:
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	OMP_NUM_THREADS=4 ./cowichan_openmp outer >> out4.txt
	

	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt
	OMP_NUM_THREADS=20 ./cowichan_openmp outer >> out20.txt

	paste out4.txt out20.txt > out.txt
	gnuplot boxplot.gnu
