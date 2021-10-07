#!/bin/bash

# Exit if any line errors
set -e

# Get the project and class names from command line args
# Probably want to make it so that we only need to provide one later
if [ $# != 2 ]
  then
    echo "Run this with two args: the project name and the class name."
    echo "E.g.: ${0} liquidity-bridge LiquidityBridge"
    exit 1
fi
PROJECT_NAME=$1
CLASS_NAME=$2

# Modify the cabal file
CABAL_FILE="${PROJECT_NAME}.cabal"
NEW_GITHUB_LINK="https://github.com/mlabs-haskell/${PROJECT_NAME}"
OLD_GITHUB_LINK="https://github.com/mlabs-haskell/CardStarter-LiquidityBridge"

mv liquidity-bridge.cabal $CABAL_FILE
sed -i "s|${OLD_GITHUB_LINK}|${NEW_GITHUB_LINK}|g" $CABAL_FILE
sed -i "s|liquidity-bridge|${PROJECT_NAME}|g" $CABAL_FILE
sed -i "s|LiquidityBridge|${CLASS_NAME}|g" $CABAL_FILE