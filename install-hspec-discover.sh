#!/bin/sh

set -ex

if [ ! -e $HOME/.local/bin/hspec-discover ]; then
  # Fetch the source
  cabal get hspec-discover-2.4.4
  cd hspec-discover-2.4.4

  # Set-up project
  echo 'packages: .' > cabal.project

  # build exe
  cabal new-build hspec-discover:exe:hspec-discover

  # copy executable to $HOME/.local/bin
  cp $(find dist-newstyle -name hspec-discover -type f) $HOME/.local/bin
fi
