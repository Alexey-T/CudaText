#!/bin/bash
ws=carbon
cud=~/cuda/cuda
exe=$cud/app/builds/macos-carbon/cudatext
bundle=$cud/app/cudatext.app
$cud/setup/mac_common.command $ws $cud $exe $bundle
