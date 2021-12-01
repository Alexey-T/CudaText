#!/bin/bash
. ./cuda_ver.sh

arch=$1
SIZE_MB=24
TMP=~/cudatext_tmp
IMG=~/cudatext_build/cudatext-macos-$arch-$cuda_ver.dmg

dd if=/dev/zero of=$IMG bs=1M count=$SIZE_MB status=progress
mkfs.hfsplus -v CudaText $IMG
mkdir -pv $TMP
sudo mount -o loop $IMG $TMP

sudo cp -a ~/cuda/cuda/app/cudatext.app $TMP/CudaText.app
sudo mkdir $TMP/CudaText.app/Contents/MacOS
sudo cp -a ~/cuda/cuda/app/builds/macos-$arch/cudatext $TMP/CudaText.app/Contents/MacOS

sudo ln -s /Applications $TMP/Applications

echo Wait 20 sec...
sleep 20
sudo umount $TMP
rmdir $TMP
