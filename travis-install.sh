set -ex

case $BUILD in
  stack*)
    mkdir -p ~/.local/bin;
    if [ `uname` = "Darwin" ]; then
      curl -kL https://www.stackage.org/stack/osx-x86_64 | tar xz --strip-components=1 --include '*/stack' -C ~/.local/bin;
    else
      curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack';
    fi

    stack --no-terminal setup

    if [ $BUILD == "stack-space-leak" ]; then
      stack build --test --fast --library-profiling --ghc-options=-rtsopts --only-dependencies
    else
      stack --no-terminal test --only-dependencies
    fi
    ;;
  cabal)
    if [ -n "$STACKAGESNAPSHOT" ]; then
      curl -sL https://www.stackage.org/$STACKAGESNAPSHOT/cabal.config | sed 's/constraints:/preferences:/' | grep -v installed > cabal.config
      head cabal.config
    fi
    cabal --version
    echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
    if [ -f $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz ]; then
      zcat $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz > $HOME/.cabal/packages/hackage.haskell.org/00-index.tar
    fi
    cabal update -v
    sed -i 's/^jobs:/-- jobs:/' ${HOME}/.cabal/config
    cabal install --constraint="integer-simple installed" --only-dependencies --enable-tests --enable-benchmarks --dry -v > installplan.txt
    sed -i -e '1,/^Resolving /d' installplan.txt; cat installplan.txt

    # check whether current requested install-plan matches cached package-db snapshot
    if diff -u installplan.txt $HOME/.cabsnap/installplan.txt; then
      echo "cabal build-cache HIT";
      rm -rfv .ghc;
      cp -a $HOME/.cabsnap/ghc $HOME/.ghc;
      cp -a $HOME/.cabsnap/lib $HOME/.cabsnap/share $HOME/.cabsnap/bin $HOME/.cabal/;
    else
      echo "cabal build-cache MISS";
      rm -rf $HOME/.cabsnap;
      mkdir -p $HOME/.ghc $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin;
      cabal install --constraint="integer-simple installed" --only-dependencies --enable-tests --enable-benchmarks;
    fi

    # snapshot package-db on cache miss
    if [ ! -d $HOME/.cabsnap ]; then
      echo "snapshotting package-db to build-cache";
      mkdir $HOME/.cabsnap;
      cp -a $HOME/.ghc $HOME/.cabsnap/ghc;
      cp -a $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin installplan.txt $HOME/.cabsnap/;
    fi
    ;;
esac
