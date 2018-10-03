#!/bin/bash

clear
echo "Building Sokoban, please wait"
sleep 1
clear
echo "Building Sokoban, please wait."
sleep 1
clear
echo "Building Sokoban, please wait.."
sleep 1
clear
echo "Building Sokoban, please wait..."
sleep 1
clear

rmdir -r build
mkdir build

stack build --copy-bins --local-bin-path build

cd build

echo "Built process complete, press Enter to execute"
read

clear
./Sokoban
echo "Quitting..."
