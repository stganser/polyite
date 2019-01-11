# Polyite: Iterative Schedule Optimization for Parallelization in the Polyhedron Model

## Description
  Polyite is a tool that iteratively optimizes the schedule of a program that is
  representable in the [Polyhedron Model](http://polyhedral.info) in order to
  exploit the computing capacity of a given multi-core hardware.

  The exploration of the schedule search space can be done either at random or
  in a guided fashion using a genetic algorithm.

  We describe the approach in detail in our TACO'2017 article [Iterative
  Schedule Optimization for Parallelization in the Polyhedron
  Model](https://stganser.bitbucket.io/taco2017/).

## Legal
  Polyite is released under MIT license.

  Polyite depends on LLVM, Clang, Polly, isl, libbarvinok and Armin Größlinger's
  implementation of Chernikova's algorithm. LLVM, Clang and Polly are released
  under the LLVM Release License or derivates of it. isl and Chernikova are
  released under MIT license. libbarvinok is GPL licensed.

## Installation and Setup
  We start from the bottom up with the dependences of Polyite and install
  everything inside the same directory, which could be your IDE's workspace.

  We explain how to get Polyite running for the benchmarks from the [Polybench
  4.1](http://web.cse.ohio-state.edu/~pouchet.2/software/polybench/) benchmark
  suite.

  ```bash
  export BASE_DIR=$PWD
  ```

### Polly/LLVM
  Polyite uses an extended version of Polly (we provide a version of 3.9.1 and
  the older version of January, 2016 that we refer to in our publications) that
  is  capable of importing schedule trees from our extended JSCOP format and
  further transforming the imported schedules, for instance by tiling them.
  Therefore, you must clone LLVM, Clang and Polly from our specially provided
  repositories.

  1. Create a root directory for LLVM
  ```bash
  mkdir llvm_root
  export LLVM_ROOT="${BASE_DIR}/llvm_root"
  ```

  2. Clone LLVM
  ```bash
  cd ${LLVM_ROOT}
  git clone https://github.com/stganser/llvm.git
  ```

  3. Get Clang
  ``` bash
  cd ${LLVM_ROOT}/llvm/tools
  git clone https://github.com/stganser/clang.git
  ```

  4. Get Polly
  ```bash
  cd ${LLVM_ROOT}/llvm/tools
  git clone https://github.com/stganser/polly.git
  ```

  5. Create a build directory for LLVM and build it using cmake
  ```bash
  mkdir ${LLVM_ROOT}/llvm_build
  cd ${LLVM_ROOT}/llvm_build
  cmake ${LLVM_ROOT}/llvm
  make
  ```

### isl Scala Bindings
  1. Make sure you have libgmp and libclang (version 3.8) (both including headers) installed on your system, as well as libtool

  2. Get and build isl
  ```bash
  cd ${BASE_DIR}
  git clone https://github.com/stganser/isl.git
  cd isl
  mkdir install
  export ISL_INSTALL="${PWD}/install"
  ./autogen.sh
  ./configure --prefix=${ISL_INSTALL} --with-jni-include=/usr/lib/jvm/default-java/include/ --with-clang=system
  make install
  ```

  3. Generate the bindings
  ```bash
  cd ${BASE_DIR}/isl/interface
  make isl-scala.jar
  cp -r java/gen src
  cp scala/src/isl/Conversions.scala src/isl
  zip -r isl-scala.jar src
  ```
  The last three steps include the source code of the bindings into the
  generated library.

### Wrapper for the isl Scala Bindings
  This subproject makes the isl Scala bindings accessible to Polyite.

  1. Clone the repository:
  ```bash
  cd ${BASE_DIR}
  git clone https://github.com/stganser/scala-isl-utils.git
  cd ${BASE_DIR}/scala-isl-utils
  export ISL_UTILS_ROOT=${BASE_DIR}/scala-isl-utils
  mkdir libs
  cp ${BASE_DIR}/isl/interface/isl-scala.jar libs
  cp ${ISL_INSTALL}/lib/libisl*so* libs
  ```

### Barvinok Library
  libbarvinok provides an implementation of Barvinok's counting algorithm, which
  can be used to determine a polyhedron's volume. Since we do not have Scala
  bindings for libbarvinok, Polyite calls a small C-binary when it needs to
  determine a polyhedron's volume.

  1. Clone the repositories:
  ```bash
  cd ${BASE_DIR}
  git clone http://repo.or.cz/barvinok.git
  cd barvinok
  git checkout barvinok-0.39
  export BARVINOK_INSTALL=barvinok/install
  cd ${BASE_DIR}
  git clone https://github.com/stganser/barvinok_binary.git
  export BARVINOK_BINARY_ROOT=${BASE_DIR}/barvinok_binary
  ```

  2. Follow the projects' build instructions and make sure that you install
    libbarvinok to `${BASE_DIR}/barvinok/install`.

### Chernikova
  This Scala library provides an implementation of Chernikova's algorithm to
  switch between the constraints representation and the generator representation
  of polyhedra.

  ```bash
  cd ${BASE_DIR}
  git clone https://github.com/stganser/chernikova.git
  export CHERNIKOVA_ROOT=${BASE_DIR}/chernikova
  ```

### Polyite
  The following steps describe how obtain Polyite itself and put everything
  together to run benchmarks from Polybench 4.1. Since we do not provide the
  configuration for a build tool, such as sbt, the easiest way to build Polyite
  is probably by using [Scala IDE](http://scala-ide.org/).

  1. Get the code:
  ```bash
  cd ${BASE_DIR}
  git clone https://github.com/stganser/polyite.git
  export POLYITE_ROOT=${BASE_DIR}/polyite
  ```
  2. Download required libraries
  ```bash
  cd ${POLYITE_ROOT}
  mkdir libs
  ```
  Now, download Scala 2.11.6 from
  [http://www.scala-lang.org/download/2.11.6.html](http://www.scala-lang.org/download/2.11.6.html)
  and copy `scala-2.11.6/lib/scala-library.jar` and
  `scala-2.11.6/lib/scala-parser-combinators_2.11-1.0.3.jar` to
  `${POLYITE_ROOT}/libs` Further, one must download [Apache Commons Lang version
  3.4](https://archive.apache.org/dist/commons/lang/binaries/) and [Apache
  Commons Math version 3.6.1](https://archive.apache.org/dist/commons/math/binaries/).
  Again, copy the downloaded JAR-files to `${POLYITE_ROOT}/libs`.

  Polyite relies on [OpenMPI](https://www.open-mpi.org/) for its distributed
  genetic algorithm. To be able to build Polyite, Java bindings for OpenMPI
  must be in Polyites classpath. Follow the instructions on
  [https://www.open-mpi.org/faq/?category=java#java_build](https://www.open-mpi.org/faq/?category=java#java_build) to build the bindings. Copy the file
  `mpi.jar` to `${POLYITE_ROOT}/libs`. Polyite works with OpenMPI version 2.1.1.

  3. Import the projects polyite and scala-isl-utils in Scala IDE/ Eclipse and build everything. Make sure that the libraries in `${POLYITE_ROOT}/libs` are in your build path, as well as `${ISL_UTILS_ROOT}/libs/isl-scala.jar`. Make sure, that your Scala compiler assumes Scala version 2.11. Make chernikova and scala-isl-utils depend on the downloaded Scala version as well and make chernikova depend on scala-isl-utils.

  4. To use Polyite, one must execute one of the following scripts, depending on
    the desired execution mode. The scripts assume that you have OpenJDK 8
    installed in `/usr/lib/jvm/java-1.8.0-openjdk-amd64/` (the default location
    on Debian based systems).
    * `run_genetic_opt` to execute the genetic algorithm
    * `run_rand_exploration` to execute random exploration
    * `run_rand_exploration_letsee` to execute random exploration using the
    search space construction described in Pouchet et al., PLDI'08.

    The script `print_scop_size` extracts several SCoP metrics during benchmark
    preparation.

    In each of the scripts, set
    * `ISL_UTILS_LOC` to the value of `${ISL_UTILS_ROOT}`
    * `POLYITE_LOC` to the value of `${POLYITE_ROOT}`
    * `CHERNIKOVA_LOC` to the value of `${CHERNIKOVA_ROOT}`

  5. To compile and execute a schedule in order to determine its profitability,
    Polyite starts the script `measure_polybench.bash`. At the top of script,
    set `POLLY_INSTALL_DIR` to the value of `${LLVM_ROOT}/llvm_build`.

  6. The scripts in `${POLYITE_ROOT}/polybench_scripts/` are used to prepare
    Polybench 4.1 benchmarks for optimization with Polyite. They generate
    prepared LLVM-IR code, execute baseline measurements, compute reference
    output and generate configuration files for random exploration or
    optimization with the genetic algorithm.
    * In `measureBaseline.bash` set `POLLY_INSTALL_DIR` to the value of
      `${LLVM_ROOT}/llvm_build`.
    * In `prepare_benchmarks.bash` set `POLLY_INSTALL_DIR` to the value of
      `${LLVM_ROOT}/llvm_build` and `POLYITE_LOC` to the value of
      `{POLYITE_ROOT}`. This script can later be called to prepare a list of
      Polybench 4.1 benchmarks. You may want to adapt the values of the default
      configuration files that the script generates for each benchmark. Compile
      `${POLYITE_ROOT}/config_help.tex` to get a documentation of Polyite's   
      configuration options.
    * The file `polly_configurations.txt` contains a list of Polly configurations
    that `prepare_benchmarks.bash` considers during the baseline measurements.

    Install `libpapi` version 5.4.3.

  7. Download [Polybench 4.1](https://sourceforge.net/projects/polybench/files/polybench-c-4.1.tar.gz/download) and unpack the archive to `${POLYITE_ROOT}/polybench-c-4.1`.

  8. Create symbolic links in `polybench-c-4.1`:
  ```bash
  cd ${POLYITE_ROOT}/polybench-c-4.1
  ln -s ../polybench_scripts/baselineCollectData.bash baselineCollectData.bash
  ln -s ../polybench_scripts/polly_configurations.txt polly_configurations.txt
  ln -s ../polybench_scripts/benchmarks.txt benchmarks.txt
  ln -s ../polybench_scripts/collectAllBaselineResults.bash collectAllBaselineResults.bash
  ln -s ../polybench_scripts/generateRefOut.bash generateRefOut.bash
  ln -s ../polybench_scripts/measureBaseline.bash measureBaseline.bash
  ln -s ../polybench_scripts/prepare_benchmarks.bash prepare_benchmarks.bash
  ```

## Usage
  Most tools that are part of Polyite will print a help message if invoked without command line parameters.

  To prepare benchmark gemm perform the following steps:
  ```bash
  cd ${POLYITE_ROOT}/polybench-c-4.1
  ./prepare_benchmarks.bash true false false gemm
  ```
  This creates the directory `polybench-c-4.1/gemm` with the following content:
  ```
  config_ga_gemm_kernel_gemm_%entry.split---%for.end40.properties
  config_rand_gemm_kernel_gemm_%entry.split---%for.end40.properties
  gemm.c
  gemm.h
  kernel_gemm___%entry.split---%for.end40.jscop
  polybench.c
  polybench.h
  ref_output
  ```
  There are

  * configuration files for random exploration and optimization with the genetic algorithm. To understand and modify these, compile and read `${POLYITE_ROOT}/config_help.tex`.
  * All source files that are required to compile the benchmark.
  * A JSCOP file that contains the model of the SCoP to optimize.
  * A file containing reference output that was produced by a binary compiled
    with -O0.


  To change the code regions to optimize (Polyite can optimize one SCoP at a
  time) or change data set sizes, edit the file
  `polybench-c-4.1/benchmarks.txt`.

  Now, you can run schedule optimization with the genetic algorithm:
  ```bash
  cd ${POLYITE_ROOT}
  ./run_genetic_opt polybench-c-4.1/gemm/kernel_gemm___%entry.split---%for.end40.jscop polybench-c-4.1/gemm/config_ga_gemm_kernel_gemm_%entry.split---%for.end40.properties
  ```
  Depending on your configuration, this produces one JSON file per generation of
  the genetic algorithm, one CSV file and a directory containing one JSCOP file
  per schedule for manual application of the generated schedules. The JSON files
  contain all attributes of the generated schedules and can be read by Polyite,
  for instance, in order to restart an interrupted run of the genetic algorithm
  or to generate further generations.

  Analogously, random exploration can be run, using

  ```bash
  cd ${POLYITE_ROOT}
  ./run_rand_exploration polybench-c-4.1/gemm/kernel_gemm___%entry.split---%for.end40.jscop polybench-c-4.1/gemm/config_rand_gemm_kernel_gemm_%entry.split---%for.end40.properties
  ```

  To use SLURM for the evaluation of your schedules, put something like the following into your configuration file:
  ```bash
  numMeasurementThreads=42
  measurementCommand=srun -Aduckdonald -pthebigcluster --constraint=fastest_cpu_available_plx --mem=32768 --exclusive -t31 -n1 ${POLYITE_ROOT}/measure_polybench.bash
  ```
  It is important to use `srun`, since Polyite must be able into interactively
  communicate with the benchmarking script via STDIN and STDOUT. Polyite can
  pass a given [numactl](https://github.com/numactl/numactl) configuration to
  the benchmarking script.

### Strip Mining (aka Pre-Vectorization)
In the evaluation of our papers about Polyite, pre-vectorization by Polly was
disabled (`-polly-vectorizer=none`). At least in Polly 3.8/3.9 Polyite will
break Polly's schedule tree optimization, as the pre-vectorizer operates on any
schedule tree band node that has a coincident member. Yet, Polly seems to
expect that coincident members occur only in band nodes whose iteration domain
contains a single statement. We provide the configuration option `expectPrevectorization` which, if set to `true`, will cause Polyite to process
schedule trees that will not break Polly's pre-vectorizer. This feature is experimental and is only meant as a basic support for pre-vectorization with
Polly. It is unclear, whether Polyite is suitable for a schedule search space
exploration with pre-vectorization enabled in its current state.

Suggestions for improvements are welcome :-)

### Schedule Classification
  Polyite has the ability to learn performance models from the results of
  previous iterative optimization runs. It can use these models to speed up
  iterative optimization process by using a classifier that can identify likely
  unprofitable schedules. To learn the classifier, Polyite relies on Python 3.5
  and [scikit-learn](http://scikit-learn.org/stable/) version 0.19.2. Follow
  their instructions for the installation of scikit-learn and its dependencies
  via `pip`.

  We provide tools for the generation of training data for the classifier. The
  data must originate from runs of Polyites random exploration or genetic
  algorithm during which benchmarking had been used to assess schedules fitness.

  The tool `merge_populations` can be used to merge sets of schedules and
  their evaluation results. These could for instance be the populations of a run
  of the genetic algorithm. Polyite writes each of these to a separate file. Merging schedules into a single file that belong to different SCoPs is impossible. `merge_populations` as the following command line interface:

  ```bash
  ./merge_populations <JSCOP file> <output JSON file> <output CSV file> <num. execution time measurements> <population files>
  ```
  1. A JSCOP file that represents the SCoP
  2. The output JSON file that will contain the merged set of schedules.
  3. A CSV file in Polyite's output CSV format that contains isl union map
    representations of the schedules in the merged set of schedules together
    with the results from benchmarking the schedules.
  4. The number of execution time measurements per schedule during the
      generating of the input data.
  5. The JSON files containing the input data.


  The tool `fitness_calculator` labels schedules and calculates their feature
  vectors. It has the following command line ínterface:
  ```bash
  ./fitness_calculator <JSCOP file> <configuration file> <input CSV file> <output CSV file>
  ```
  1. A JSCOP file that models the SCoP to which the schedules in the input CSV
  file corresponds.
  2. A CSV file contain the schedules to be labeled together with their measured execution times.
  3. The output CSV file that Polyite will use to train its classifier.

  Polyite can train its classifier using training data from multiple CSV files.
  Read the documentation of Polyite's configuration options for details.
<hr />
&copy; 2017-2019, Stefan Ganser
