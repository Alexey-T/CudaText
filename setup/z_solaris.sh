#!/bin/sh

. ./cuda_ver.sh
os=solaris
widgets=gtk2
cpu=amd64

outdir=~/cudatext_build
outdir_tmp=$outdir/tmp_solaris

# make .tar.xz

./unix_common.sh $os $widgets $cpu builds/solaris-x64-gtk2 $outdir


# make .zip too

mkdir -p $outdir_tmp
file_xz=$outdir/cudatext-$os-$widgets-$cpu-$cuda_ver.tar.xz
file_zip=$outdir/cudatext-$os-$widgets-$cpu-$cuda_ver.zip

tar -xf $file_xz -C $outdir_tmp
cd $outdir_tmp
zip -r $file_zip ./*
cd ..
rm -rf $outdir_tmp
