

# Tamanho da entrada
DATA= -DSMALL_DATA
#DATA= -DMEDIUM_DATA
#DATA= -DBIG_DATA

cowichan_openmp:	cowichan.o
	g++ -Wall -m64  -fgnu-tm -fopenmp $(DATA) -D LIN64 -O2 -o cowichan_openmp *.cpp cowichan.o tinySTM/lib/libstm.a

cowichan.o: ../cowichan/cowichan.cpp
	g++ -Wall -m64  -fgnu-tm -fopenmp $(DATA) -D LIN64 -O2 -c ../cowichan/cowichan.cpp

clean:
	rm *.o

clean_all:	
	rm *.o *.txt *.png

gnu:
	gnuplot outerboxplot-DSMALL_DATA.gnu
	gnuplot outerboxplot-DMEDIUM_DATA.gnu
	gnuplot outerboxplot-DBIG_DATA.gnu
	gnuplot normboxplot-DSMALL_DATA.gnu
	gnuplot normboxplot-DMEDIUM_DATA.gnu
	gnuplot normboxplot-DBIG_DATA.gnu
	gnuplot vecdiffboxplot-DSMALL_DATA.gnu
	gnuplot vecdiffboxplot-DMEDIUM_DATA.gnu
	gnuplot vecdiffboxplot-DBIG_DATA.gnu
	gnuplot sorboxplot-DSMALL_DATA.gnu
	gnuplot sorboxplot-DMEDIUM_DATA.gnu
	gnuplot sorboxplot-DBIG_DATA.gnu
	gnuplot hullboxplot-DSMALL_DATA.gnu
	gnuplot hullboxplot-DMEDIUM_DATA.gnu
	gnuplot hullboxplot-DBIG_DATA.gnu
	gnuplot threshboxplot-DSMALL_DATA.gnu
	gnuplot threshboxplot-DMEDIUM_DATA.gnu
	gnuplot threshboxplot-DBIG_DATA.gnu


outer:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 30) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp outer >> outerOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	gnuplot outerboxplot$(DATA).gnu

norm:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 30) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp norm >> normOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	gnuplot normboxplot$(DATA).gnu

vecdiff:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 30) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp vecdiff >> vecdiffOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	gnuplot vecdiffboxplot$(DATA).gnu	


sor:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 30) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp sor >> sorOut$(DATA)$$t.txt ; \
        done ; \
    done ;
	gnuplot sorboxplot$(DATA).gnu

hull:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 30) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp hull >> hullOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	gnuplot hullboxplot$(DATA).gnu


thresh:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 30) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp thresh >> threshOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	gnuplot threshboxplot$(DATA).gnu

all:
	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 30) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp outer >> outerOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	#gnuplot outerboxplot$(DATA).gnu

	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 30) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp norm >> normOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	#gnuplot normboxplot$(DATA).gnu

	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 30) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp vecdiff >> vecdiffOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	#gnuplot vecdiffboxplot$(DATA).gnu

	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 30) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp sor >> sorOut$(DATA)$$t.txt ; \
        done ; \
    done ;
	#gnuplot sorboxplot$(DATA).gnu

	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 30) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp hull >> hullOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	#gnuplot hullboxplot$(DATA).gnu

	for t in 2 4 8 16 32 64 ; do \
        #n=1 ; while [ $${n} -le 5 ] ; do \
		for n in $$(seq 1 30) ; do \
			OMP_NUM_THREADS=$$t ./cowichan_openmp thresh >> threshOut$(DATA)$$t.txt ; \
        done ; \
    done ;

	#gnuplot threshboxplot$(DATA).gnu