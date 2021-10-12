# plutus-scaffold
This project contains the build systems and scripts needed to scaffold a plutus project quickly.

## Running
To get everything set up, copy the contents of this repo to your new project. Then run `./setup.sh <project-name> <ModuleName>`, e.g., `./setup.sh gero-gov GeroGov`.

## Potential Issues
* If you submit your pull request, but get an error on the GitHub CIs saying something to the effect of "Binary cache mlabs doesn't exist or it's private," or that MLabs.cachix.com doesn't exist, then the cachix key is not setup, and you or whoever owns your repository will have to add that.
* If you get an error saying that "The package directory './.' does not contain any .cabal file," then you probably should either start with a fresh repository with no commits, or you should make a commit with git and rerun `nix-build nix/ci.nix`. This issue arises because nix is looking in your .git folder to try and identify what your .cabal file is, and since that has been renamed, nix seems to assume that there is no .cabal file. Starting with a fresh repo causes it to search the directory for your .cabal file, and making a commit changes the file name in the .git folder.
