#!/bin/bash

POLLY_INSTALL_DIR="set this to the correct path"
clang="${POLLY_INSTALL_DIR}/bin/clang"
llc="${POLLY_INSTALL_DIR}/bin/llc"
opt="${POLLY_INSTALL_DIR}/bin/opt"
llvm_link="${POLLY_INSTALL_DIR}/bin/llvm-link"
polly="${POLLY_INSTALL_DIR}/bin/clang"
POLYITE_LOC="set this to the correct path"

pollyConfigurations="`pwd`/polly_configurations.txt"

pollyAAFlags='-basicaa -scev-aa'
timeFlag='-DPOLYBENCH_TIME'
papiFlag='-DPOLYBENCH_PAPI'
numBaseLineMeasurementRuns=5

function getKernelFuncName {
    kernelFunctionName="kernel_`echo ${benchmarkName} | sed 's/-/_/g'`"
}

function extractRegionBoundaries {
    local regionStr=$1
    regionEntryPoint=`echo "${region}" | sed -r 's/^%//g' | sed -r 's/---.*$//g'`
    regionExitPoint=`echo "${region}" | sed 's/^.*---%//g'`
}

function getRegionList {
  regionsTmp=/tmp/regions_tmp
  for f in *jscop
  do
    grep -P '\"name\" : \".* => .*\",' < ${f} | sed -r 's/\s+\"name\"\s:\s\"(.*)\s=>\s(.*)\",/%\1---%\2/g'
  done > ${regionsTmp}
}

function getScopSize {
        local benchmarkName=$1
        local fName=$2
        local regionId=$3
        local jscopFile=`ls ${benchmarkName} | grep ${fName} | grep ${regionId} | grep jscop`
        local tmpFile=/tmp/scopSizeFile
        ${POLYITE_LOC}/print_scop_size ${benchmarkName}/${jscopFile} > ${tmpFile}
        scopNDeps=`grep 'n deps' < ${tmpFile} | sed 's/n deps: //g'`
        scopNStmts=`grep 'n statements' < ${tmpFile} | sed 's/n statements: //g'`
        scopMaxLoopDepth=`grep 'max loop depth' < ${tmpFile} | sed 's/max loop depth: //g'`
        scopNStructurePar=`grep 'n structure param' < ${tmpFile} | sed 's/n structure param: //g'`
        interfShare=`grep 'interf share' < ${tmpFile} | sed 's/interf share: //g'`
        rm ${tmpFile}
}

function writeGAConfig {
    local benchmarkName=$1
    local kernelFunction=$2
    local region=$3
    local regionStart=$4
    local regionEnd=$5
    local output=$6
    local paramValMappings=$7

    echo "numMeasurementThreads=30" > ${output}
    echo "regularPopulationSize=30" >> ${output}
    echo "maxNumNewSchedsFromCrossover=3" >> ${output}
    echo "rayCoeffsRange=3" >> ${output}
    echo "lineCoeffsRange=3" >> ${output}
    echo "maxNumRays=2" >> ${output}
    echo "maxNumLines=2" >> ${output}
    echo "probabilityToCarryDep=0.4" >> ${output}
    echo "maxNumSchedsAtOnce=2" >> ${output}
    echo "probabilityToMutateSchedRow=0.1" >> ${output}
    echo "probabilityToMutateGeneratorCoeff=0.1" >> ${output}
    echo "generatorCoeffMaxDenominator=3" >> ${output}
    echo "measurementCommand=${POLYITE_LOC}/measure_polybench.bash" >> ${output}
    echo "measurementWorkingDir=${POLYITE_LOC}/" >> ${output}
    echo "measurementTmpDirBase=/tmp/" >> ${output}
    echo "measurementTmpDirNamePrefix=${benchmarkName}" >> ${output}
    echo "benchmarkName=${benchmarkName}" >> ${output}
    echo "functionName=${kernelFunction}" >> ${output}
    echo "scopRegionStart=${regionStart}" >> ${output}
    echo "scopRegionEnd=${regionEnd}" >> ${output}
    echo "irFilesLocation=`pwd`" >> ${output}
    echo "referenceOutputFile=`pwd`/${benchmarkName}/ref_output" >> ${output}
    echo "numExecutionTimeMeasurements=5" >> ${output}
    echo "populationFilePrefix=${benchmarkName}_population" >> ${output}
    echo "currentGeneration=0" >> ${output}
    echo "maxGenerationToReach=40" >> ${output}
    echo "fractionOfSchedules2Keep=(1 / 2)" >> ${output}
    echo "exportSchedulesToJSCOPFiles=true" >> ${output}
    echo "jscopFolderPrefix=${benchmarkName}_population" >> ${output}
    echo "csvFilePrefix=${benchmarkName}_population" >> ${output}
    echo "measurementTimeout=1800" >> ${output}
    echo "exportPopulationToCSV=true" >> ${output}
    echo "logToFile=true" >> ${output}
    echo "logFile=genetic_algorithm_${benchmarkName}.log" >> ${output}
    echo "evaluationSigIntExitCode=42" >> ${output}
    echo "optimizeParallelExecTime=true" >> ${output}
    echo "filterImportedPopulation=false" >> ${output}
    echo "evolutionTimeout=180" >> ${output}
    echo "randSchedsTimeout=200" >> ${output}
    echo "shareOfRandSchedsInPopulation=(1 / 20)" >> ${output}
    echo "genSchedsMaxAllowedConseqFailures=20" >> ${output}
    echo "numScheduleGenThreads=7" >> ${output}
    echo "islComputeout=38400000" >> ${output}
    echo "paramValMappings=${paramValMappings}" >> ${output}
    echo "measureParExecTime=true" >> ${output}
    echo "measureSeqExecTime=false" >> ${output}
    echo "replaceDimsEnabled=true" >> ${output}
    echo "replacePrefixEnabled=true" >> ${output}
    echo "replaceSuffixEnabled=true" >> ${output}
    echo "mutateGeneratorCoeffsEnabled=true" >> ${output}
    echo "geometricCrossoverEnabled=true" >> ${output}
    echo "rowCrossoverEnabled=true" >> ${output}
    echo "initPopulationNumLines=2" >> ${output}
    echo "initPopulationNumRays=2" >> ${output}
    echo "moveVertices=false" >> ${output}
    echo "rayPruningThreshold=NONE" >> ${output}
    echo "seqPollyOptFlags=-mllvm -polly-position=early -mllvm -polly-process-unprofitable=true -mllvm -polly-parallel=false -mllvm -polly-tiling=false -mllvm -polly-vectorizer=none  -mllvm -polly-dependences-computeout=0 -D${dataSetSize} -DPOLYBENCH_USE_C99_PROTO" >> ${output}
    echo "parPollyOptFlags=-mllvm -polly-position=early -mllvm -polly-process-unprofitable=true -mllvm -polly-parallel=true -mllvm -polly-tiling=true -mllvm -polly-default-tile-size=64 -mllvm -polly-vectorizer=none -mllvm -polly-dependences-computeout=0 -D${dataSetSize} -DPOLYBENCH_USE_C99_PROTO" >> ${output}
    echo "insertSetNodes=false" >> ${output}
    echo "useConvexAnnealingFunction=false" >> ${output}
    echo "compilationTimeout=300" >> ${output}
    echo "benchmarkingSurrenderTimeout=$((20*60))" >> ${output}
    echo "measureCacheHitRatePar=false" >> ${output}
    echo "measureCacheHitRateSeq=false" >> ${output}
    echo "seed=NONE" >> ${output}
    echo "numactlConf=--physcpubind=0-7 --membind=0" >> ${output}
    echo "linIndepVectsDoNotFixDims=false" >> ${output}
    echo "simplifySchedTrees=true" >> ${output}
    echo "splitLoopBodies=false" >> ${output}
    echo "numCompilatonDurationMeasurements=0" >> ${output}
    echo "validateOutput=true" >> ${output}
    echo "tilingPermitInnerSeq=false" >> ${output}
    echo "schedTreeSimplRebuildDimScheds=false" >> ${output}
    echo "schedTreeSimplRemoveCommonOffset=true" >> ${output}
    echo "schedTreeSimplDivideCoeffsByGCD=true" >> ${output}
    echo "schedTreeSimplElimSuperfluousSubTrees=true" >> ${output}
    echo "schedTreeSimplElimSuperfluousDimNodes=true" >> ${output}
    echo "barvinokBinary=${HOME}/workspace/count_integer_points/count_integer_points" >> ${output}
    echo "barvinokLibraryPath=${HOME}/workspace/barvinok/barvinok/install/lib" >> ${output}
    echo "normalizeFeatures=true" >> ${output}
    echo "evaluationStrategy=CPU" >> ${output}
    echo "learningSet=" >> ${output}
    echo "decTreeMinSamplesLeaf=15" >> ${output}
    echo "learningAlgorithm=CART" >> ${output}
    echo "randForestNTree=100" >> ${output}
    echo "randForestMaxFeatures=8" >> ${output}
    echo "executionMode=SINGLE_PROCESS" >> ${output}
    echo "pythonVEnvLocation=NONE" >> ${output}
    echo "samplingStrategy=CHERNIKOVA" >> ${output}
    echo "schedCoeffsMin=-4" >> ${output}
    echo "schedCoeffsMax=4" >> ${output}
    echo "schedCoeffsExpectationValue=1.43" >> ${output} # 1 / 0.7
    echo "migrationRate = 1" >> ${output}
    echo "migrationVolume = (1 / 2)" >> ${output}
    echo "scheduleEquivalenceRelation = RATIONAL_MATRIX_AND_GENERATORS" >> ${output}
    echo "schedCoeffsAbsMax = 3" >> ${output}
    echo "gaCpuTerminationCriterion = FIXED_NUM_GENERATIONS" >> ${output}
    echo "convergenceTerminationCriterionWindowSize = 10" >> ${output}
    echo "convergenceTerminationCriterionThreshold = 0.025" >> ${output}
    echo "expectPrevectorization=false" >> ${output}
}

function writeRandExpConf {
    local benchmarkName=$1
    local kernelFunction=$2
    local region=$3
    local regionStart=$4
    local regionEnd=$5
    local output=$6
    local paramValMappings=$7

    echo "numMeasurementThreads=30" > ${output}
    echo "rayCoeffsRange=3" >> ${output}
    echo "lineCoeffsRange=3" >> ${output}
    echo "maxNumRays=2" >> ${output}
    echo "maxNumLines=2" >> ${output}
    echo "probabilityToCarryDep=0.4" >> ${output}
    echo "maxNumSchedsAtOnce=2" >> ${output}
    echo "measurementCommand=${POLYITE_LOC}/measure_polybench.bash" >> ${output}
    echo "measurementWorkingDir=${POLYITE_LOC}" >> ${output}
    echo "measurementTmpDirBase=/tmp/" >> ${output}
    echo "measurementTmpDirNamePrefix=${benchmarkName}_rand" >> ${output}
    echo "benchmarkName=${benchmarkName}" >> ${output}
    echo "functionName=${kernelFunction}" >> ${output}
    echo "scopRegionStart=${regionStart}" >> ${output}
    echo "scopRegionEnd=${regionEnd}" >> ${output}
    echo "irFilesLocation=`pwd`" >> ${output}
    echo "referenceOutputFile=`pwd`/${benchmarkName}/ref_output" >> ${output}
    echo "numExecutionTimeMeasurements=5" >> ${output}
    echo "populationFilePrefix=${benchmarkName}_rand" >> ${output}
    echo "exportSchedulesToJSCOPFiles=true" >> ${output}
    echo "jscopFolderPrefix=${benchmarkName}_rand" >> ${output}
    echo "csvFilePrefix=${benchmarkName}_rand" >> ${output}
    echo "measurementTimeout=1800" >> ${output}
    echo "exportPopulationToCSV=true" >> ${output}
    echo "logToFile=true" >> ${output}
    echo "logFile=rand_exploration_${benchmarkName}.log" >> ${output}
    echo "evaluationSigIntExitCode=42" >> ${output}
    echo "randSchedsTimeout=200" >> ${output}
    echo "genSchedsMaxAllowedConseqFailures=1000" >> ${output}
    echo "numScheds=$((30 + 40*15))" >> ${output}
    echo "numScheduleGenThreads=7" >> ${output}
    echo "filterImportedPopulation=false" >> ${output}
    echo "importScheds=false" >> ${output}
    echo "islComputeout=38400000" >> ${output}
    echo "barvinokBinary=${HOME}/workspace/count_integer_points/count_integer_points" >> ${output}
    echo "barvinokLibraryPath=${HOME}/workspace/barvinok/barvinok/install/lib" >> ${output}
    echo "paramValMappings=${paramValMappings}" >> ${output}
    echo "measureParExecTime=true" >> ${output}
    echo "measureSeqExecTime=false" >> ${output}
    echo "boundSchedCoeffs=true" >> ${output}
    echo "moveVertices=false" >> ${output}
    echo "rayPruningThreshold=NONE" >> ${output}
    echo "seqPollyOptFlags=-mllvm -polly-position=early -mllvm -polly-process-unprofitable=true -mllvm -polly-parallel=false -mllvm -polly-tiling=false -mllvm -polly-vectorizer=none -mllvm -polly-dependences-computeout=0 -D${dataSetSize} -DPOLYBENCH_USE_C99_PROTO" >> ${output}
    echo "parPollyOptFlags=-mllvm -polly-position=early -mllvm -polly-process-unprofitable=true -mllvm -polly-parallel=true -mllvm -polly-dependences-computeout=0 -mllvm -polly-tiling=true -mllvm -polly-default-tile-size=64 -mllvm -polly-vectorizer=none -D${dataSetSize} -DPOLYBENCH_USE_C99_PROTO" >> ${output}
    echo "insertSetNodes=false" >> ${output}
    echo "compilationTimeout=300" >> ${output}
    echo "benchmarkingSurrenderTimeout=$((20*60))" >> ${output}
    echo "measureCacheHitRatePar=false" >> ${output}
    echo "measureCacheHitRateSeq=false" >> ${output}
    echo "seed=NONE" >> ${output}
    echo "numactlConf=--physcpubind=0-7 --membind=0" >> ${output}
    echo "completeSchedules=false" >> ${output}
    echo "linIndepVectsDoNotFixDims=false" >> ${output}
    echo "simplifySchedTrees=true" >> ${output}
    echo "splitLoopBodies=false" >> ${output}
    echo "evaluateScheds=true" >> ${output}
    echo "numCompilatonDurationMeasurements=0" >> ${output}
    echo "validateOutput=true" >> ${output}
    echo "tilingPermitInnerSeq=false" >> ${output}
    echo "schedTreeSimplRebuildDimScheds=false" >> ${output}
    echo "schedTreeSimplRemoveCommonOffset=true" >> ${output}
    echo "schedTreeSimplDivideCoeffsByGCD=true" >> ${output}
    echo "schedTreeSimplElimSuperfluousSubTrees=true" >> ${output}
    echo "schedTreeSimplElimSuperfluousDimNodes=true" >> ${output}
    echo "numSchedTreeSimplDurationMeasurements=NONE" >> ${output}
    echo "normalizeFeatures=true" >> ${output}
    echo "evaluationStrategy=CPU" >> ${output}
    echo "pythonVEnvLocation=NONE" >> ${output}
    echo "depsWeighingMethod=STMT_MEM_TRAFFIC" >> ${output}
    echo "samplingStrategy=CHERNIKOVA" >> ${output}
    echo "schedCoeffsMin=-4" >> ${output}
    echo "schedCoeffsMax=4" >> ${output}
    echo "schedCoeffsExpectationValue=1.43" >> ${output} # 1 / 0.7
    echo "scheduleEquivalenceRelation = RATIONAL_MATRIX_AND_GENERATORS" >> ${output}
    echo "schedCoeffsAbsMax = 3" >> ${output}
    echo "normalizeFeatures=true" >> ${output}
    echo "learningSet=" >> ${output}
    echo "decTreeMinSamplesLeaf=15" >> ${output}
    echo "learningAlgorithm=CART" >> ${output}
    echo "randForestNTree=100" >> ${output}
    echo "randForestMaxFeatures=8" >> ${output}
    echo "executionMode=SINGLE_PROCESS" >> ${output}
    echo "pythonVEnvLocation=NONE" >> ${output}
    echo "expectPrevectorization=false" >> ${output}
}

if [ ${#} -lt 2 ]
then
    echo "$0 <generate reference output> [use slurm] [slurm config]
        <generate baseline> [use numactl] [numactl config] <list of benchmarks>
    <generate reference output> indicates whether reference output should be
                                generated
    [use slurm]                 Must be set iff <generate reference output>  is
                                set. Indicates whether SLURM should be used to
                                generate the reference output
    [slurm config]              Must be set iff <use slurm> is true and provides
                                the slurm configurations for generating the
                                reference ouput.
    <generate baseline>         Uses a hard-coded SLURM configuration. Is
                                ignored if <generate reference output> is not
                                true.
    <use numactl>               Iff <generate baseline> is true, you must choose
                                whether numactl should be used during baseline
                                measurement.
    <numactl config>            Configuration for numactl. Must be provided iff
                                <use numactl> is true.
    <list of benchmarks>        must contain at least one element"
    exit 1
fi

i=1
genRefOutput=${!i}
i=$((i + 1))
if [ ${genRefOutput} == "true" ]
then
    useSlurm=${!i}
    i=$((i + 1))
    if [ ${useSlurm} == "true" ]
    then
        slurmConfig=${!i}
        i=$((i + 1))
    fi
    generateBaseline=${!i}
    i=$((i + 1))
else
    generateBaseline="false"
fi

if [ ${generateBaseline} == "true" ]
then
    useNumactl=${!i}
    i=$((i + 1))
    if [ ${useNumactl} == "true" ]
    then
       numactlConfig=${!i}
       i=$((i + 1))
    fi
fi

k=0
for ((j=i; j <= ${#}; ++j))
do
    benchmarks[${k}]=${!j}
    k=$((k + 1))
done

selectedBenchmarksTmp=/tmp/selectedBenchmarks.txt
printf "" > ${selectedBenchmarksTmp}
for benchmark in ${benchmarks[@]}
do
    cat benchmarks.txt | grep -P "^${benchmark}\\t" >> ${selectedBenchmarksTmp}
    if [ $? -ne 0 ]
    then
        echo "Benchmark ${benchmark} does not exist."
    fi
done

while read benchmarkName benchmarkPath benchmarkSrc dataSetSize
do
    echo "generating ${benchmarkName}."

    base_dir=`pwd`
    mkdir -p ${benchmarkName}
    cd ${benchmarkName}

    echo "Generating linked and canonicalized IR for ${benchmarkName}."

    function getCompileStr () {
        local binaryName=$1
        local flags=$2
        compileStr="${polly} ${flags} -DPOLYBENCH_USE_C99_PROTO -D${dataSetSize} -I../utilities \
-I../${benchmarkPath} ../utilities/polybench.c ../${benchmarkSrc} -lm -lgomp \
-o ${binaryName}"
    }

    function compile () {
        local binaryName=$1
        local flags=$2
        getCompileStr "${binaryName}" "${flags}"
        ${compileStr}
    }

    function generateIRAndLink () {
        local extraFlags1=$1
        local resultFile1=$2
        local tmpPolybench=/tmp/polybench_tmp.ll
        local tmpBench=/tmp/benchmark_tmp.ll
        ${clang} -S -emit-llvm -I ../utilities -D${dataSetSize} \
-DPOLYBENCH_USE_C99_PROTO ${extraFlags1} ../utilities/polybench.c -o ${tmpPolybench}
        ${clang} -S -emit-llvm -I ../utilities -I ../${benchmarkPath} \
-D${dataSetSize} -DPOLYBENCH_USE_C99_PROTO ${extraFlags1} \../${benchmarkSrc} \
-o ${tmpBench}
        ${llvm_link} -S ${tmpPolybench} ${tmpBench} > ${resultFile1}
        rm ${tmpPolybench} ${tmpBench}
    }

    function generateIRAndLinkAndCanonicalize () {
        local extraFlags=$1
        local resultFile=$2
        local tmpLinked=/tmp/linked.ll
        generateIRAndLink "${extraFlags}" "${tmpLinked}"
        ${opt} -polly-canonicalize ${tmpLinked} > ${resultFile}
        rm ${tmpLinked}
    }

#    generateIRAndLinkAndCanonicalize "" ${benchmarkName}.preopt.ll
#    generateIRAndLinkAndCanonicalize "${timeFlag}" ${benchmarkName}.preopt.ll.time
#    generateIRAndLinkAndCanonicalize "-DPOLYBENCH_DUMP_ARRAYS"\
#     ${benchmarkName}.preopt.ll.dump_arrays
#    generateIRAndLinkAndCanonicalize ${papiFlag} ${benchmarkName}.preopt.ll.papi

    echo "Copying benchmark source files for ${benchmarkName}."
    cp ../${benchmarkPath}/${benchmarkName}.* ../utilities/polybench.c ../utilities/polybench.h .

    getKernelFuncName

    echo "Exporting the JSCOP file(s) for ${benchmarkName}"
    ${polly} -march=native -O3 -mllvm -polly -mllvm -polly-export -mllvm -polly-only-func=${kernelFunctionName} -mllvm -polly-dependences-computeout=0 -mllvm -polly-optimizer=none -I. -DPOLYBENCH_USE_C99_PROTO -D${dataSetSize} polybench.c ${benchmarkName}.c -o /dev/null

    if [ ${genRefOutput} == "true" ]
    then
        echo "Generating reference output"
        tmpDumpArraysBin=tmp_dump_arrays
        compile ${tmpDumpArraysBin} "-O0 -DPOLYBENCH_DUMP_ARRAYS"
        if [ ${useSlurm} == "true" ]
        then
            echo "Using SLURM to generate the reference output for ${benchmarkName}."
            genRefOutputJobId=`sbatch ${slurmConfig} --parsable ../generateRefOut.bash | sed 's/;.*//g'`
        else
            echo "Generating the reference output for ${benchmarkName} locally."
            ./${tmpDumpArraysBin} 2> ref_output
            rm ${tmpDumpArraysBin}
        fi
    else
        echo "Skipping reference output generation."
    fi

    echo "Generating the baseline for ${benchmarkName}."
    getRegionList
    polybenchDFlags="-DPOLYBENCH_USE_C99_PROTO -D${dataSetSize}"
    if [ ${generateBaseline} == "true" ]
    then
        binaryCount=0
        echo "Scheduling baseline measurement for O3"
        binaryCount=$((binaryCount + 1))
        printf "${benchmarkName};_;_;O3;${polybenchDFlags};${useNumactl};${numactlConfig}\n" > baselineMeasurementTasks
    fi

    while read region
    do
        extractRegionBoundaries ${region}
        getKernelFuncName
        while read pollyConfigName pollyConfig
        do
            pollyConfCompl="${pollyConfig} -mllvm -polly-only-func=${kernelFunctionName} \
-mllvm -polly-only-region=${regionEntryPoint} ${polybenchDFlags}"
            if [ ${generateBaseline} == "true" ]
            then
                echo "Scheduling baseline measurement for polly configuration ${pollyConfigName}"
                binaryCount=$((binaryCount + 1))
                printf "${benchmarkName};${kernelFunctionName};${regionEntryPoint};\
${pollyConfigName};${pollyConfCompl};${useNumactl};${numactlConfig}\n" >> baselineMeasurementTasks
            fi
        done < ${pollyConfigurations}
    done < ${regionsTmp}
    if [ ${generateBaseline} == "true" ]
    then
        if [ -z "${genRefOutputJobId}" ]
        then
            jobId=`sbatch --array=1-${binaryCount} --parsable ../measureBaseline.bash | sed 's/;.*//g'`
        else
            jobId=`sbatch --array=1-${binaryCount} --parsable  --dependency=afterany:${genRefOutputJobId} ../measureBaseline.bash | sed 's/;.*//g'`
        fi
        sbatch --dependency=afterany:${jobId} ../baselineCollectData.bash
        rm ${regionsTmp}
    fi
    cd ${base_dir}
done < ${selectedBenchmarksTmp}

echo "Writing the benchmark configurations."
benchmarkConfigs="`pwd`/benchmark_configs.csv"
printf "benchmark\tbaseDir\tirFilePrefix\tkernelFunc\tregion\tregionEntryPoint\tconfigFileGA\tconfigFileRand\tnStmts\tnDeps\tmaxLoopDepth\tnStructureParams\tinterfShare\n" \
> ${benchmarkConfigs}

while read benchmarkName benchmarkPath benchmarkSrc dataSetSize
do
    echo "Adding ${benchmarkName} to ${benchmarkConfigs}."
    getKernelFuncName
    cd ${benchmarkName}
    getRegionList
    cd ..

    # extract the data set sizes
    paramValMappings=`cpp -I utilities -I${benchmarkPath} ${benchmarkSrc} -D${dataSetSize} -DPOLYBENCH_USE_C99_PROTO | sed -ne '/int main/,$p' | grep -P 'int [A-Za-z0-9-_]+ = ' | sed -r 's/int ([A-Za-z0-9_]+) = ([0-9]+);/\1=\2/g' | xargs | sed -r 's/\s+/,/g'`

    while read region
    do
        extractRegionBoundaries ${region}
        gaConfigFile="${benchmarkName}/config_ga_${benchmarkName}_${kernelFunctionName}_${region}.properties"
        writeGAConfig ${benchmarkName} ${kernelFunctionName} ${region} ${regionEntryPoint} ${regionExitPoint} ${gaConfigFile} ${paramValMappings}
        randConfigFile="${benchmarkName}/config_rand_${benchmarkName}_${kernelFunctionName}_${region}.properties"
        writeRandExpConf ${benchmarkName} ${kernelFunctionName} ${region} ${regionEntryPoint} ${regionExitPoint} ${randConfigFile} ${paramValMappings}
        getScopSize ${benchmarkName-} ${kernelFunctionName} ${region}
        printf "${benchmarkName}\t`pwd`/${benchmarkName}/\t\
${benchmarkName}.preopt.ll\t${kernelFunctionName}\t`echo $region | sed 's/%/%%/g'`\t\
${regionEntryPoint}\t`realpath ${gaConfigFile} | sed 's/%/%%/g'`\t`realpath ${randConfigFile} | sed 's/%/%%/g'`\t${scopNStmts}\t${scopNDeps}\t${scopMaxLoopDepth}\t${scopNStructurePar}\t${interfShare}\n" >> ${benchmarkConfigs}
    done < ${regionsTmp}
    rm ${regionsTmp}
done < ${selectedBenchmarksTmp}

rm ${selectedBenchmarksTmp}
