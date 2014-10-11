#!/bin/sh
mkdir -p priv && g++ -g c_src/valgrind-tests.cc -o priv/valgrind-tests
cd deps/p1_xml && ./configure --enable-full-xml --enable-nif && make clean all
