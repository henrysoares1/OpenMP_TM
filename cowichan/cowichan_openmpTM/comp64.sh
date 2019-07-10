#!/bin/sh
g++ -Wall -m64  -fgnu-tm -fopenmp -D LIN64 -O2 -c ../cowichan/cowichan.cpp
g++ -Wall -m64  -fgnu-tm -fopenmp -D LIN64 -O2 -o cowichan_openmptm *.cpp cowichan.o
