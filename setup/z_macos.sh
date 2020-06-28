#!/bin/bash
. ./cuda_ver.sh

SIZE_MB=22
TMP=~/m
IMG=~/cudatext-macos-$cuda_ver.dmg

dd if=/dev/zero of=$IMG bs=1M count=$SIZE_MB status=progress
mkfs.hfsplus -v CudaText $IMG
mkdir -pv $TMP
sudo mount -o loop $IMG $TMP

sudo cp -a ~/cuda/cuda/app/cudatext.app $TMP/CudaText.app
sudo mkdir $TMP/CudaText.app/Contents/MacOS
sudo cp -a ~/cuda/cuda/app/builds/macos-cocoa/cudatext $TMP/CudaText.app/Contents/MacOS

sudo ln -s /Applications $TMP/Applications

sleep 20
sudo umount $TMP
