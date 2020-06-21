#!/usr/bin/env bash

echo "Installing bruc..."
echo "## stack build"

stack build

echo "## copy to path"

sudo cp ./.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.0.1.0/build/bruc/bruc /usr/bin/

echo "## cleaning up"

stack clean

echo "## finished!"
