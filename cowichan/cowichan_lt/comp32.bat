g++ -Wall -m32 -Os -Iinclude -c ../cowichan/cowichan.cpp
g++ -Wall -m32 -Os -llinuxtuples -Wno-write-strings -I../linuxtuples-1.03 -o cowichan_lt src/*.cpp cowichan.o
