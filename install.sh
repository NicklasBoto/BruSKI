#!/bin/bash

# create config file
read -p "Create new config (yes if first install)? [y/n]: " yn
case $yn in
        [Yy]*)  echo "Setting prelude path...";
                echo "PATH: $PWD/src/Prelude/"; echo;
                echo "module Config (preludePath, arityBlock) where" > src/Config.hs;
                echo "-- Path to prelude" >> src/Config.hs;
                echo "preludePath = \"$PWD/src/Prelude/\"" >> src/Config.hs;
                echo "-- Block arities higher than the number of binders" >> src/Config.hs;
                echo "arityBlock = False" >> src/Config.hs;;
                
        *)  echo "Skipping...\n";;
esac

# install package
echo  Installing with stack...

stack install --verbosity error

if [ "$?" -gt 0 ]; then
        echo "Stack error..."
        echo "Please contact mail@nicklasbotö.se"
else
        echo "Executable 'bruc' installed to $HOME/.local/bin/bruc"
        echo "Be sure to add ~/.local/bin/ to your PATH."
        echo "Done!"
fi

