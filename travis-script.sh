set -e

case $BUILD in
  stack)
    stack --no-terminal test github
    stack --no-terminal build github-samples

    # TODO: get executables from info
    for testbin in show-user list-followers list-following operational; do
        echo "Running " $testbin
        stack exec github-$testbin
    done
    ;;
  cabal)
    if [ -f configure.ac ]; then autoreconf -i; fi
    cabal configure --constraint="integer-simple installed" --enable-tests --enable-benchmarks -v2  # -v2 provides useful information for debugging
    cabal build   # this builds all libraries and executables (including tests/benchmarks)
    cabal test --show-details=always

    if [ "$CABALVER" = "1.22" ]; then cabal check; fi
    if [ "$CABALVER" = "1.22" ]; then cabal haddock; fi

    cabal sdist   # tests that a source-distribution can be generated

    # Check that the resulting source distribution can be built & installed.
    # If there are no other `.tar.gz` files in `dist`, this can be even simpler:
    # `cabal install --force-reinstalls dist/*-*.tar.gz`
    export SRCPKG=$(cabal info . | awk '{print $2;exit}')
    cd dist
    tar -xzvf $SRCPKG.tar.gz
    cd $SRCPKG
    cabal configure --enable-tests && cabal build && cabal install --force-reinstalls
    ;;
esac
