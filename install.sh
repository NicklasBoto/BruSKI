#!/usr/bin/env bash

echo "Installing bruc..."
echo "Bulding..."

stack build

echo "Copying to path..."

sudo cp ./.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.0.1.0/build/bruc/bruc /usr/bin/

echo "Cleaning up..."

stack clean

echo "Finished!"
