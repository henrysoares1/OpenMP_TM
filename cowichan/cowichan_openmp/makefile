cowichan_openmp:	cowichan.o
	g++ -Wall -m64 -fopenmp -D LIN64 -O2 -o cowichan_openmp *.cpp cowichan.o

cowichan.o: ../cowichan/cowichan.cpp
	g++ -Wall -m64 -fopenmp -D LIN64 -O2 -c ../cowichan/cowichan.cpp

clean:	
	rm *.o

run:
	export OMP_NUM_THREADS=4
	$(info executando vecdiff com 4 threads)
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff

	export OMP_NUM_THREADS=20
	$(info executando vecdiff com 20 threads)
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
	./cowichan_openmp vecdiff
