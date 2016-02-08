#!/bin/sh

# Use this script for execute portable version of CudaText

cd "`dirname "$0"`"

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$(pwd)

./cudatext