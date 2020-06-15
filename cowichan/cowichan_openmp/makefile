

# Tamanho da entrada
DATA= -DSMALL_DATA
#DATA= -DMEDIUM_DATA
#DATA= -DBIG_DATA

cowichan_openmp:	cowichan.o
	g++ -Wall -m64 -fopenmp $(DATA) -D LIN64 -O2 -o cowichan_openmp *.cpp cowichan.o

cowichan.o: ../cowichan/cowichan.cpp
	g++ -Wall -m64 -fopenmp $(DATA) -D LIN64 -O2 -c ../cowichan/cowichan.cpp

clean:
	rm *.o

clean_all:	
	rm *.o *.txt *.png

outer:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp outer >> outerOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	gnuplot outerboxplot$(DATA).gnu

norm:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp norm >> normOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	gnuplot normboxplot$(DATA).gnu

vecdiff:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp vecdiff >> vecdiffOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	gnuplot vecdiffboxplot$(DATA).gnu	


sor:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp sor >> sorOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	gnuplot sorboxplot$(DATA).gnu

hull:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp hull >> hullOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	gnuplot hullboxplot$(DATA).gnu


thresh:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp thresh >> threshOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	gnuplot threshboxplot$(DATA).gnu

all:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp outer >> outerOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	#gnuplot outerboxplot$(DATA).gnu

	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp norm >> normOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	#gnuplot normboxplot$(DATA).gnu

	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp vecdiff >> vecdiffOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	#gnuplot vecdiffboxplot$(DATA).gnu

	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp sor >> sorOut$(DATA)$$t.txt ; \
        done ; \
    done ;
	#gnuplot sorboxplot$(DATA).gnu

	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp hull >> hullOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	#gnuplot hullboxplot$(DATA).gnu

	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 5) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp thresh >> threshOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	#gnuplot threshboxplot$(DATA).gnu