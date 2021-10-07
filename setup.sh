#!/bin/bash

# Get the project and class names from command line args
# Probably want to make it so that we only need to provide one later
if [ $# != 2 ]
  then
    echo "Run this with two args: the project name and the class name."
    echo "E.g.: ${0} liquidity-bridge LiquidityBridge"
    exit 1
fi
PROJECTNAME=$1
CLASSNAME=$2

# Modify the cabal file
CABALFILE="${PROJECTNAME}.cabal"
mv liquidity-bridge.cabal $CABALFILE

