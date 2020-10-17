#!/bin/bash

# create config file
read -p "Create new config (yes if first install)? [y/N]: " yn
case $yn in
        [Yy]*)  echo "Setting prelude path...";
                echo "PATH: $PWD/src/Prelude/"; echo;
                echo "module Config (preludePath, arityBlock) where" > src/Config.hs;
                echo "-- Path to prelude" >> src/Config.hs;
                echo "preludePath = \"$PWD/src/Prelude/\"" >> src/Config.hs;
                echo "-- Block arities higher than the number of binders" >> src/Config.hs;
                echo "arityBlock = False" >> src/Config.hs;;
                
        *)  echo "Skipping..."; echo;;
esac

# ask for vim installation
read -p "Install BruSKI for vim? [Y/n]: " ynvim
case $ynvim in
        [Nn]*) echo "Ok. Skipping...";;
        *) bash ./syntax/syntax.sh;;
esac

# install package
echo  Installing with stack...

stack install --verbosity error

if [ "$?" -gt 0 ]; then
        echo "Stack error..."
        echo "Please contact nicklas.boto@gmail.com"
else
        echo "Executable 'bruc' installed to $HOME/.local/bin/bruc"
        echo "Be sure to add ~/.local/bin/ to your PATH."
        echo "Done!"
fi

