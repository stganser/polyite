#!/bin/bash
#SBATCH -J pbench-collect
#SBATCH -n1
#SBATCH -N1
#SBATCH -panywhere
#SBATCH -Aanywhere
#SBATCH -t1
#SBATCH -o polybench-baseline-measurement-collect.out
#SBATCH -e polybench-baseline-measurement-collect.err

outputFile=baseline.csv

printf "benchmarkName\tkernelFunc\tregionEntryPoint\tpollyConfigName\tpollyConfig\texecTime\n" > ${outputFile}

numConfigs=cat

cat exec_times_* >> ${outputFile}