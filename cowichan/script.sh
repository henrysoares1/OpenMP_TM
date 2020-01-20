#!/bin/sh
cd cowichan_openmp/
make clean
make
make all
cd ..
cd cowichan_openmpTM/
make clean
make
make all
