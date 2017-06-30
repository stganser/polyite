#!/bin/bash
binary=tmp_dump_arrays
./${binary} 2> ref_output
rm ${binary}