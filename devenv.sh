
# source this file for some helper commands

# Run tests
function rt ()
{
    cabal-dev build && ./dist/build/test/test
}
