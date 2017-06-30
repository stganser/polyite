#!/bin/bash
#SBATCH -J pbench-baseline
#SBATCH -n1
#SBATCH -panywhere
#SBATCH -Aanywhere
#SBATCH --constraint=zeus
#SBATCH -t200
#SBATCH --exclusive
#SBATCH --mem=32768
#SBATCH -o polybench-baseline-measurement-%a.out
#SBATCH -e polybench-baseline-measurement-%a.err

POLLY_INSTALL_DIR=/scratch/ganser/polly

#opt="${POLLY_INSTALL_DIR}/bin/opt -load ${POLLY_INSTALL_DIR}/lib/LLVMPolly.so"
opt="${POLLY_INSTALL_DIR}/bin/opt"
llc="${POLLY_INSTALL_DIR}/bin/llc"

tmpDir=/scratch/ganser
tmpFileSuffix="_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}"
outputFile=exec_times_${kernelFuncName}_${regionEntryPoint}_${pollyConfigName}

if [ ! -e baselineMeasurementTasks ]
then
    echo "File baselineMeasurementTasks cannot be read. Dont't know what to do."
    exit 1
fi
numConfigs=`wc -l < baselineMeasurementTasks`

if [ ${numConfigs} -lt ${SLURM_ARRAY_TASK_ID} ]
then
    echo "There is no configuration for task number ${SLURM_ARRAY_TASK_ID}"
fi

# get config including compile line for time measurement
config=`head -n ${SLURM_ARRAY_TASK_ID} < baselineMeasurementTasks | tail -n1`
benchmarkName=`echo ${config} | awk -F\; '{print $1}'`
kernelFuncName=`echo ${config} | awk -F\; '{print $2}'`
regionEntryPoint=`echo ${config} | awk -F\; '{print $3}'`
pollyConfigName=`echo ${config} | awk -F\; '{print $4}'`
optFlags=`echo ${config} | awk -F\; '{print $5}'`
useNumactl=`echo ${config} | awk -F\; '{print $6}'`
numactlConfig=`echo ${config} | awk -F\; '{print $7}'`

binaryTime="time_binary_${SLURM_ARRAY_TASK_ID}"
binaryDumpArrays="dump_arrays_binary_${SLURM_ARRAY_TASK_ID}"

echo "benchmarkName = ${benchmarkName}"
echo "kernelFuncName = ${kernelFuncName}"
echo "regionEntryPoint = ${regionEntryPoint}"
echo "pollyConfigName = ${pollyConfigName}"
echo "binaryTime = ${binaryTime}"
echo "binaryDumpArrays = ${binaryDumpArrays}"
echo "opt flags: ${optFlags}"
echo "useNumactl = ${useNumactl}"
echo "numactl flags: ${numactlConfig}"

outputFile=exec_times_${kernelFuncName}_${regionEntryPoint}_${pollyConfigName}

printf "${benchmarkName}\t${kernelFuncName}\t${regionEntryPoint}\t${pollyConfigName}\t\
\"${optFlags}\"\t" > ${outputFile}

if [ ! -r ref_output ]
then
    echo "missing reference output" >> ${outputFile}
    exit 1
fi

function compile {
    local binaryName=$1
    local llFile=$2
${opt} ${optFlags} ${llFile} | ${opt} -march=native -O3 > ${binaryName}.opt.ll
    if [ $? -ne 0 ]
    then
        # compilation failed.
          echo "compile error" >> ${outputFile}
          exit 0
    fi
    
    ${llc} ${binaryName}.opt.ll -o ${binaryName}.s
	if [ $? -ne 0 ]
	then
	    # compilation failed.
	      echo "compile error" >> ${outputFile}
	      exit 0
	fi
	
	gcc ${binaryName}.s -lgomp -lm -o ${binaryName}
	if [ $? -ne 0 ]
    then
        # compilation failed.
          echo "compile error" >> ${outputFile}
          exit 0
    fi
}

# compile the time measurement binary
compile "${binaryTime}" "${benchmarkName}.preopt.ll.time"

# compile the output validation binary.
compile "${binaryDumpArrays}" "${benchmarkName}.preopt.ll.dump_arrays"

# validate output
tmpDumpFile=${tmpDir}/baseline_tmp_dumpfile_${tmpFileSuffix}
if [ ${useNumactl} == "true" ]
then
    srun --pstate-turbo=off numactl ${numactlConfig} ./${binaryDumpArrays} 2> ${tmpDumpFile}
else
    srun --pstate-turbo=off ./${binaryDumpArrays} 2> ${tmpDumpFile}
fi

if [ $? -ne 0 ]
then
    echo "output validation execution failed" >> ${outputFile}
    rm -f ${tmpDumpFile}
    exit 0
fi

diff ref_output ${tmpDumpFile} > /dev/null

if [ $? -ne 0 ]
then
    echo "wrong output" >> ${outputFile}
    rm -f ${tmpDumpFile}
    exit 0
fi
echo "Output validation succeeded: size of the output file: `ls -lh ${tmpDumpFile}`"
rm -f ${tmpDumpFile}

numRuns=5
tmpOutput="${tmpDir}/execTimesTmp_${tmpFileSuffix}"

for ((i=0; i < numRuns; ++i))
do
    if [ ${useNumactl} == "true" ]
    then
	    srun --pstate-turbo=off numactl ${numactlConfig} ./${binaryTime} >> ${tmpOutput}
	    if [ $? -ne 0 ]
	    then
	        echo "execution failed" >> ${outputFile}
	        exit 0
	    fi
    else
        srun --pstate-turbo=off ./${binaryTime} >> ${tmpOutput}
        if [ $? -ne 0 ]
        then
            echo "execution failed" >> ${outputFile}
            exit 0
        fi
    fi
done
sort -n < ${tmpOutput} | head -n1 >> ${outputFile}
rm -f ${tmpOutput}