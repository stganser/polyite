#!/bin/bash
#POLLY_INSTALL_DIR=/scratch/ganser/polly
POLLY_INSTALL_DIR=/home/stg/Documents/arbeit/polly/llvm_build
#opt="${POLLY_INSTALL_DIR}/bin/opt -load ${POLLY_INSTALL_DIR}/lib/LLVMPolly.so"
opt="${POLLY_INSTALL_DIR}/bin/opt"
llc="${POLLY_INSTALL_DIR}/bin/llc"
echo "Started"
nArgs=20
function printerr {
    local s=${1}
    echo ${s} > /dev/stderr
}
if [ ${#} -lt ${nArgs} ]
then
    printerr "Wrong number of arguments (expected ${nArgs}): found ${#}"
    printerr "List of expected arguments"
    printerr "    - workerThreadID"
    printerr "    - tmpDirBase"
    printerr "    - benchmarkName"
    printerr "    - functionName"
    printerr "    - scopRegionStart"
    printerr "    - scopRegionEnd"
    printerr "    - referenceOutputFile"
    printerr "    - numCompilatonDurationMeasurements"
    printerr "    - validateOutputEnabled"
    printerr "    - numExecutionTimeMeasurements"
    printerr "    - irFilesLocation"
    printerr "    - sigIntExitCode"
    printerr "    - measureCacheHitRatePar"
    printerr "    - measureCacheHitRateSeq"
    printerr "    - measureParExecTime"
    printerr "    - measureSeqExecTime"
    printerr "    - seqPollyOptFlags"
    printerr "    - parPollyOptFlags"
    printerr "    - useNumactl"
    printerr "    - numactlConf (use only if useNumactl is true)"
    exit 1
fi
i=1
workerThreadID=${!i}
i=$((i + 1))
tmpDirBase=`echo ${!i} | sed 's/\/$//g'`
#tmpDirBase=`realpath ${tmpDirBase}`
i=$((i + 1))
measurementTmpDirNamePrefix=${!i}
i=$((i + 1))
benchmarkName=${!i}
i=$((i + 1))
functionName=${!i}
i=$((i + 1))
scopRegionStart=${!i}
i=$((i + 1))
scopRegionEnd=${!i}
i=$((i + 1))
referenceOutputFile=${!i}
#referenceOutputFile=`realpath ${referenceOutputFile}`
i=$((i + 1))
numCompilatonDurationMeasurements=${!i}
i=$((i + 1))
validateOutputEnabled=${!i}
i=$((i + 1))
numExecutionTimeMeasurements=${!i}
i=$((i + 1))
irFilesLocation=`echo ${!i} | sed 's/\/$//g'`
i=$((i + 1))
sigIntExitCode=${!i}
i=$((i + 1))
measureCacheHitRatePar=${!i}
i=$((i + 1))
measureCacheHitRateSeq=${!i}
i=$((i + 1))
measureParExecTime=${!i}
i=$((i + 1))
measureSeqExecTime=${!i}
i=$((i + 1))
seqPollyOptFlags=${!i}
i=$((i + 1))
parPollyOptFlags=${!i}
i=$((i + 1))
useNumactl=${!i}
if [ ${useNumactl} == "true" ]
then
    i=$((i + 1))
    numactlConf=${!i}
fi
irFile="${irFilesLocation}/${benchmarkName}/${benchmarkName}.preopt.ll"
irFileTime="${irFile}.time"
irFileValidateOutput="${irFile}.dump_arrays"
irFilePapi="${irFile}.papi"
# validate args 1
if [ ! -d ${tmpDirBase} ] || [ ! -w ${tmpDirBase} ] || [ ! -x ${tmpDirBase} ]
then
    printerr "${tmpDirBase} is not an existing searchable and writable \
directory."
    exit 1
fi

ls -ld ${tmpDirBase} > /dev/stderr

function checkStringNotEmpty {
    local str=$1
    local paramName=$2
    if [ -z ${str} ]
    then
        printerr "${paramName} must be set."
        exit 1
    fi
}
function checkIsBoolean {
    local str=$1
    local paramName=$2
    if [ ! ${str} == "true" ] && [ ! ${str} == "false" ]
    then
        printerr "${paramName} must either be true or false: ${str}"
        exit 1
    fi
}
checkStringNotEmpty ${benchmarkName} "benchmarkName"
checkStringNotEmpty ${functionName} "functionName"
checkStringNotEmpty ${scopRegionStart} "scopRegionStart"
checkStringNotEmpty ${scopRegionEnd} "scopRegionEnd"
checkStringNotEmpty ${measureCacheHitRatePar} "measureCacheHitRatePar"
checkStringNotEmpty ${measureCacheHitRateSeq} "measureCacheHitRateSeq"
checkStringNotEmpty ${measureParExecTime} "measureParExecTime"
checkStringNotEmpty ${measureSeqExecTime} "measureSeqExecTime"
checkStringNotEmpty ${seqPollyOptFlags} "seqPollyOptFlags"
checkStringNotEmpty ${parPollyOptFlags} "parPollyOptFlags"
checkStringNotEmpty ${validateOutputEnabled} "validateOutputEnabled"
checkStringNotEmpty ${numCompilatonDurationMeasurements} "numCompilatonDurationMeasurements"
checkStringNotEmpty ${numExecutionTimeMeasurements} "numExecutionTimeMeasurements"
checkIsBoolean ${measureCacheHitRatePar} "measureCacheHitRatePar"
checkIsBoolean ${measureCacheHitRateSeq} "measureCacheHitRateSeq"
checkIsBoolean ${measureParExecTime} "measureParExecTime"
checkIsBoolean ${measureSeqExecTime} "measureSeqExecTime"
checkIsBoolean ${useNumactl} "useNumactl"
checkIsBoolean ${validateOutputEnabled} "validateOutputEnabled"
if [ ${useNumactl} == "true" ]
then
    checkStringNotEmpty ${numactlConf} "numactlConf"
fi
for f in ${irFile} ${irFileTime} ${irFileValidateOutput}
do
    if [ ! -r ${f} ]
    then
        printerr "IR file ${f} cannot be read."
        exit 1
    fi
done
if [ ! -r ${referenceOutputFile} ]
then
    printerr "${referenceOutputFile} is not a readable file."
    exit 1
fi
region="%${scopRegionStart}---%${scopRegionEnd}"
pollyAAFlags='-basicaa -scev-aa'
pollyFuncRegionFlags="-polly-only-func=${functionName} -polly-only-region=${scopRegionStart}"
${opt} ${pollyAAFlags} -polly-scops ${pollyFuncRegionFlags} -analyze -q ${irFile} \
| grep 'Region:' | grep ${region} > /dev/null
if [ $? -ne 0 ]
then
    printerr "The region ${region} is not a valid SCoP."
    exit 0
fi
# create the tmp dir
workingDir="${measurementTmpDirNamePrefix}_schedule-opt-worker${workerThreadID}"
tmpDir="${tmpDirBase}/${workingDir}"
rm -rf ${tmpDir}
mkdir ${tmpDir}
if [ ! -d ${tmpDir} ] || [ ! -w ${tmpDir} ] || [ ! -x ${tmpDir} ]
then
    printerr "${tmpDir} could not be created."
    exit 1
fi
cd ${tmpDir}
function cleanupAndExit {
    exitCode=$1
    for pid in `jobs -p`
    do
        pkill -9 -P ${pid}
        kill -9 ${pid}
    done
    cd ${tmpDirBase}
    rm -rf ${workingDir}
    exit ${exitCode}
}
function _trap {
    cleanupAndExit ${sigIntExitCode}
}
trap _trap SIGINT SIGTERM
kernelFuncName=`echo ${benchmarkName} | sed 's/-/_/g'`
jscopFile="kernel_${kernelFuncName}___${region}.jscop"
while read line
do
    echo ${line} >> ${jscopFile}
done
# test whether the schedule is valid
${opt} ${pollyAAFlags} -polly-import-jscop -polly-import-jscop-read-schedule-tree=true \
${pollyFuncRegionFlags} -analyze ${irFile} > schedImportOut 2>&1 &
pid=$!
wait ${pid}
if [ $? -ne 0 ]
then
    printerr 'Failed to execute opt'
    while read line
    do
        printerr ${line}
    done < schedImportOut
    echo 'false'
    cleanupAndExit 1
fi
echo 'true'
# generate code for output validation, time measurement and optionally cache hit rate measurement
function checkFileExists {
    fileName=$1
    if [ ! -r ${fileName} ]
    then
        printerr "${fileName} is not a readable file."
        echo 'false'
        cleanupAndExit 1
    fi
}
function checkExecutableExists {
    fileName=$1
    if [ ! -r ${fileName} ]
    then
        printerr "${fileName} is not an executable"
        echo 'false'
        cleanupAndExit 1
    fi
}
function compile {
    local ir=$1
    local prefix=$2
    local makeParallel=$3
    local pollyFlags="-S ${pollyAAFlags} -polly-import-jscop \
-polly-import-jscop-read-schedule-tree=true -polly-codegen ${pollyFuncRegionFlags}"
    if [ ${makeParallel} == "true" ]
    then
        pollyFlags="${pollyFlags} ${parPollyOptFlags}"
    else
        pollyFlags="${pollyFlags} ${seqPollyOptFlags}"
    fi
    pollyFlags="${pollyFlags} -march=native"
    local optLLFile="${prefix}.opt.ll"
    local optLLFileO3="${prefix}.optO3.ll"
    # Polly
    # measure the duration of code generation
    unset compileDurations

    for ((i = 0; i < numCompilatonDurationMeasurements; ++i))
    do
        if [ ${useNumactl} == "true" ]
        then
            /usr/bin/time -f%e -ocompileTimeOut numactl ${numactlConf} ${opt} ${pollyFlags} ${ir} > /dev/null &
        else
            /usr/bin/time -f%e -ocompileTimeOut ${opt} ${pollyFlags} ${ir} > /dev/null &
        fi
        pid=$!
	    wait ${pid}
	    if [ $? -ne 0 ]
	    then
	        printerr 'OPT failed'
	        echo 'false'
	        cleanupAndExit 1
	    fi
	    local currCompileDuration=`cat compileTimeOut`
	    compileDurations[${i}]=${currCompileDuration}
	    rm compileTimeOut
    done
    ${opt} ${pollyFlags} ${ir} > ${optLLFile} &
    pid=$!
    wait ${pid}
    if [ $? -ne 0 ]
    then
        printerr 'OPT failed'
        echo 'false'
        cleanupAndExit 1
    fi
    checkFileExists ${optLLFile}
    # O3
    ${opt} -march=native -O3 < ${optLLFile} > ${optLLFileO3} &
    pid=$!
    wait ${pid}
    if [ $? -ne 0 ]
    then
        printerr 'OPT -O3 failed'
        echo 'false'
        cleanupAndExit 1
    fi
    checkFileExists ${optLLFileO3}
    sFile=${prefix}.opt.s
    ${llc} ${optLLFileO3} -o ${sFile} &
    pid=$!
    wait ${pid}
    if [ $? -ne 0 ]
    then
        printerr 'LLC failed'
        echo 'false'
        cleanupAndExit 1
    fi
    checkFileExists ${sFile}
    gcc-5 ${sFile} -lgomp -lm -lpapi -o ${prefix} &
    pid=$!
    wait ${pid}
    if [ $? -ne 0 ]
    then
        printerr 'GCC failed'
        echo 'false'
        cleanupAndExit 1
    fi
    checkExecutableExists ${prefix}
}
timeMeasureBinaryPar='time_par'
arrayDumpBinaryPar='dump-arrays_par'
timeMeasureBinarySeq='time_seq'
arrayDumpBinarySeq='dump-arrays_seq'
cacheHitRateMeasureBinarySeq='cache-hit-measure_seq'
cacheHitRateMeasureBinaryPar='cache-hit-measure_par'
if [ ${measureParExecTime} == "true" ]
then
    compile ${irFileTime} ${timeMeasureBinaryPar} true
    parCompileDurations=${compileDurations[*]}
    if [ ${validateOutputEnabled} == "true" ]
    then
        compile ${irFileValidateOutput} ${arrayDumpBinaryPar} true
    fi
fi
if [ ${measureSeqExecTime} == "true" ]
then
    compile ${irFileTime} ${timeMeasureBinarySeq} false
    seqCompileDurations=${compileDurations[*]}
    if [ ${validateOutputEnabled} == "true" ]
    then
        compile ${irFileValidateOutput} ${arrayDumpBinarySeq} false
    fi
fi
if [ ${measureCacheHitRatePar} == "true" ]
then
    compile ${irFilePapi} ${cacheHitRateMeasureBinaryPar} true
fi
if [ ${measureCacheHitRateSeq} == "true" ]
then
    compile ${irFilePapi} ${cacheHitRateMeasureBinarySeq} false
fi
echo 'true'
if [ ${measureParExecTime} == "true" ]
then
    echo ${parCompileDurations}
fi
if [ ${measureSeqExecTime} == "true" ]
then
    echo ${seqCompileDurations}
fi
# test whether the output produced by the generated code matches the reference
# output
function validateOutput {
    local binary=$1
    local outputFile='output'
    if [ ${useNumactl} == "true" ]
    then
        numactl ${numactlConf} ./${binary} 2> ${outputFile} &
        local pid=$!
        wait ${pid}
        if [ $? -ne 0 ]
        then
            printerr 'the output validation run failed.'
            echo 'false'
            cleanupAndExit 1
        fi
    else
        ./${binary} 2> ${outputFile} &
        local pid=$!
        wait ${pid}
        if [ $? -ne 0 ]
        then
            printerr 'the output validation run failed.'
            echo 'false'
            cleanupAndExit 1
        fi
    fi
    if [ ! -r ${outputFile} ]
    then
        printerr "the output file wasn't written"
        echo 'false'
        cleanupAndExit 1
    fi
    echo 'true'
    diff ${outputFile} ${referenceOutputFile} > /dev/null &
    pid=$!
    wait ${pid}
    if [ $? -ne 0 ]
    then
        printerr "The output doesn't match the reference output"
        echo 'false'
        cleanupAndExit 1
    fi
    echo 'true'
}
if [ ${measureParExecTime} == "true" ] && [ ${validateOutputEnabled} == "true" ]
then
    validateOutput ${arrayDumpBinaryPar}
fi
if [ ${measureSeqExecTime} == "true" ] && [ ${validateOutputEnabled} == "true" ]
then
    validateOutput ${arrayDumpBinarySeq}
fi
# measure the execution time of the generated code numExecutionTimeMeasurements
# times
function measureExecutionTime {
    local binary=$1
    local t
    for((i=0; i < numExecutionTimeMeasurements; i++))
    do
        local timeOutput=timeLog
        if [ ${useNumactl} == "true" ]
        then
            numactl ${numactlConf} ./${binary} > ${timeOutput} &
            local pid=$!
            wait ${pid}
            if [ $? -ne 0 ]
            then
                echo 'false'
                cleanupAndExit 1
            fi
        else
            ./${binary} > ${timeOutput} &
            local pid=$!
            wait ${pid}
            if [ $? -ne 0 ]
            then
                echo 'false'
                cleanupAndExit 1
            fi
        fi
        local tCurr=`cat ${timeOutput}`
        if [[ ! ${tCurr} =~ ^[0-9]+(\.[0-9]*)?$ ]]
        then
            echo 'false'
            cleanupAndExit 1
        fi
        t[${i}]=${tCurr}
    done
    echo 'true'
    echo ${t[*]}
}
if [ ${measureParExecTime} == "true" ]
then
    measureExecutionTime ${timeMeasureBinaryPar}
fi
if [ ${measureSeqExecTime} == "true" ]
then
    measureExecutionTime ${timeMeasureBinarySeq}
fi
# measure the cache hit rate
function measureCacheHitRate {
    local binary=$1
    local t
    local timeOutput=timeLog
    if [ ${useNumactl} == "true" ]
    then
        numactl ${numactlConf} ./${binary} > ${timeOutput} &
        local pid=$!
        wait ${pid}
        if [ $? -ne 0 ]
        then
            echo 'false'
            cleanupAndExit 1
        fi
    else
        ./${binary} > ${timeOutput} &
        local pid=$!
        wait ${pid}
        if [ $? -ne 0 ]
        then
            echo 'false'
            cleanupAndExit 1
        fi
    fi
    local tCurr=`cat ${timeOutput}`
    arrIN=(${tCurr//;/ })
    maxAccess=$((${arrIN[1]} + ${arrIN[2]}))
    cacheHitRate=$(awk "BEGIN { print ${arrIN[0]}/${maxAccess} }")
    echo 'true'
    echo ${cacheHitRate}
}
if [ ${measureCacheHitRatePar} == "true" ]
then
    measureCacheHitRate ${cacheHitRateMeasureBinaryPar}
fi
if [ ${measureCacheHitRateSeq} == "true" ]
then
    measureCacheHitRate ${cacheHitRateMeasureBinarySeq}
fi
cleanupAndExit 0
