#!/bin/bash

for i in {1..5}; do
	echo Running Test $i
	./compiler 15_A5_quads$i < ./A5_tests/test$i.nc
done