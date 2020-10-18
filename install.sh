#!/bin/bash

# create config file
echo "Creating config..."

_PRELUDE="$PWD/src/Prelude/"

if grep "preludePath = " src/Config.hs > /dev/null
then
        echo "Config exists, skipping..."
else
        echo "-- Path to prelude" >> src/Config.hs
        echo "preludePath = \"$_PRELUDE\"" >> src/Config.hs;
fi

# ask for vim installation
echo
read -p "Install BruSKI for vim? [y/N]: " _yn
case $_yn in
        [Yy]*) cd syntax; 
               bash syntax.sh;
               cd ..;;
        *) echo "Ok. Skipping...";;
esac

# install package
echo -e "\nInstalling with stack..."

if stack install --verbosity error
then
        echo "Executable 'bruc' installed to $HOME/.local/bin/bruc"
        echo "Be sure to add ~/.local/bin/ to your PATH."
        echo "Done!"
else
        echo "Stack error..."
        echo "Please contact bruski@nicklasbotö.se"
fi

