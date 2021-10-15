#!/usr/bin/env bash

if [[ $# -ne 1 ]]; then
    echo "Incorrect usage: update-plutus.sh {ref}" >&2
    exit 2
fi

PLUTUS_CABAL=$(curl -s https://raw.githubusercontent.com/input-output-hk/plutus/$1/cabal.project)

if [[ $PLUTUS_CABAL == "404: Not Found" ]]; then
    echo "No cabal.project file could be found for ref: \"$1\"" >&2
    exit 2
fi

INDEX=$(echo "$PLUTUS_CABAL" | head -2)

CONTENTS=$'\npackages: ./. \n\nsource-repository-package\n  type: git\n  location: https://github.com/input-output-hk/plutus.git\n  tag: '$1$'\n  subdir:'

PROJECT_START=$(echo "$PLUTUS_CABAL" | grep -n -m 1 "packages:" | cut -d: -f1)

DIRS=$(echo "$PLUTUS_CABAL" | tail -n +"$PROJECT_START")
DIRS=${DIRS:9}

NEW_CABAL=$INDEX$CONTENTS$DIRS

echo "$NEW_CABAL" > './cabal.project'
echo "Successfully updated cabal.project, updating sources..."

sh ./update-sha256map.sh
