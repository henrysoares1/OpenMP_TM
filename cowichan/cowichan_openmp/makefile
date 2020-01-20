cowichan_openmp:	cowichan.o
	g++ -Wall -m64 -fopenmp -D LIN64 -O2 -o cowichan_openmp *.cpp cowichan.o

cowichan.o: ../cowichan/cowichan.cpp
	g++ -Wall -m64 -fopenmp -D LIN64 -O2 -c ../cowichan/cowichan.cpp

clean:	
	rm *.o *.txt *.png

outer:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp outer >> outerOut$$t.txt ; \
        done ; \
    done ;

	gnuplot outerboxplot.gnu

norm:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp norm >> normOut$$t.txt ; \
        done ; \
    done ;

	gnuplot normboxplot.gnu

vecdiff:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp vecdiff >> vecdiffOut$$t.txt ; \
        done ; \
    done ;

	gnuplot vecdiffboxplot.gnu	


sor:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp sor >> sorOut$$t.txt ; \
        done ; \
    done ;
	gnuplot sorboxplot.gnu

hull:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp hull >> hullOut$$t.txt ; \
        done ; \
    done ;

	gnuplot hullboxplot.gnu


thresh:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp thresh >> threshOut$$t.txt ; \
        done ; \
    done ;

	gnuplot threshboxplot.gnu

all:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp outer >> outerOut$$t.txt ; \
        done ; \
    done ;

	#gnuplot outerboxplot.gnu

	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp norm >> normOut$$t.txt ; \
        done ; \
    done ;

	#gnuplot normboxplot.gnu

	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp vecdiff >> vecdiffOut$$t.txt ; \
        done ; \
    done ;

	#gnuplot vecdiffboxplot.gnu

	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp sor >> sorOut$$t.txt ; \
        done ; \
    done ;
	#gnuplot sorboxplot.gnu

	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp hull >> hullOut$$t.txt ; \
        done ; \
    done ;

	#gnuplot hullboxplot.gnu

	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp thresh >> threshOut$$t.txt ; \
        done ; \
    done ;

	#gnuplot threshboxplot.gnu