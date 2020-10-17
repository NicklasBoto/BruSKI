#!/bin/bash

# create config file
echo "Creating config..."

PRELUDE="$HOME/.config/bruski/prelude/"

if grep "preludePath = " src/Config.hs > /dev/null
then
        echo "Config exists, skipping..."
else
        install -D src/Prelude/* $PRELUDE
        echo "-- Path to prelude" >> src/Config.hs
        echo "preludePath = \"$PRELUDE\"" >> src/Config.hs;
fi

# ask for vim installation
echo
read -p "Install BruSKI for vim? [Y/n]: " ynvim
case $ynvim in
        [Nn]*) echo "Ok. Skipping...";;
        *) cd syntax; 
           bash syntax.sh;
           cd ..;
esac

# install package
echo
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

