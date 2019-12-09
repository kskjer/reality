#!/bin/bash
# notes....
#    (cd ~/Documents/Projects/mupen64/mupen64plus-core && git diff origin/master..) | git apply --directory=refactor/ext/mupen64plus-core/
#    git subtree add --prefix refactor/ext/mupen64plus-core git@github.com:mupen64plus/mupen64plus-core.git ef15526ecdcde8f984a6162f9b874aa87108a3d4 --squash

#if [[ -z `git remote get-url m64p-core` ]]
#then 
#	git remote add m64p-core git@github.com:mupen64plus/mupen64plus-core.git
#fi


#git fetch m64p-core && git merge --squash -s recursive -Xsubtree=refactor/ext/mupen64plus-core/ --allow-unrelated-histories m64p-core/master

(cd ../.. && git subtree pull --prefix refactor/ext/mupen64plus-core git@github.com:mupen64plus/mupen64plus-core.git master --squash)
