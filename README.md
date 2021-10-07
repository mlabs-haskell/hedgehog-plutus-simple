# plutus-scaffold
This project contains the build systems and scripts needed to scaffold a plutus project quickly.

## Running
To get everything set up, copy the contents of this repo to your new project. Then run `./setup.sh <project-name> <ModuleName>`, e.g., `./setup.sh gero-gov GeroGov`.

## Potential Issues
* If you submit your pull request, but get an error on the GitHub CIs saying something to the effect of "Binary cache mlabs doesn't exist or it's private," or that MLabs.cachix.com doesn't exist, then the cachix key is not setup, and you or whoever owns your repository will have to add that.
