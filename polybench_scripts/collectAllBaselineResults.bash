#!/bin/bash

outputFile=baseline-all.csv

printf "benchmarkName\tkernelFunc\tregionEntryPoint\tpollyConfigName\tpollyConfig\texecTime\n" > ${outputFile}

for f in `find | grep 'baseline.csv'`
do
tail -n+2 < ${f} >> ${outputFile}
done