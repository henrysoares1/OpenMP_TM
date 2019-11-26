#!/bin/sh
g++ -Wall -m64  -fgnu-tm -fopenmp -D LIN64 -O2 -c ../cowichan/cowichan.cpp
g++ -Wall -m64  -fgnu-tm -fopenmp -D LIN64 -O2 tinySTM/lib/libstm.a -o cowichan_openmptm *.cpp cowichan.o
