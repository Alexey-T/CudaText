#!/bin/bash

# update submodules
git submodule foreach git pull origin master

# update upstream
git pull upstream master

bash "$(dirname -- "${BASH_SOURCE[0]}")"/build.sh
