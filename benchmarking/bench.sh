#!/bin/bash
set -x

for ITER in `seq 0 9`
do
    for SIZE_TRIALS in 16K-30000 100-80000 128K-8000
    do
	SIZE_TRIALS=`echo $SIZE_TRIALS | tr "-" " "`
	read SIZE TRIALS <<< "$SIZE_TRIALS"
	for THREADS in 5 15 50 150 275 370 450 500 100 200 300 400
	do
	    for PORT in 80 8080 81
	    do
		weighttp -t 4 -c ${THREADS} -n ${TRIALS} http://gs11701.sp.cs.cmu.edu:${PORT}/bench-files/TEST_${SIZE} > data/test_${ITER}_${SIZE}_${THREADS}_${PORT} 2>&1 
	    done
	done
    done
done
