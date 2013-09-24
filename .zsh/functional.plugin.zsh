#!/bin/zsh

# https://github.com/Tarrasch/zsh-functional

# Load functions
for file in $(dirname $0)/functional/{map,each,filter,fold,foldright,loop-magic,docs}.zsh
do
  . $file
done
