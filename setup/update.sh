#!/bin/bash

# update submodules
git submodule foreach git pull origin master

# list git submodules changed
for r in `git submodule status | grep '^+' | cut -f2 -d' '`; do
	# commit to master with commit summary
	git commit $r -m "Merge branch 'master' of `git -C $r remote get-url origin`
`git submodule summary $r | tail -n +2`"
done

# update upstream
git pull upstream master

# run the build script
bash "$(dirname -- "${BASH_SOURCE[0]}")"/build.sh
