set -e

source setup-env.sh

# Run cobl-* unittests.
export COB_LIBRARY_PATH=build
export COB_PRE_LOAD="cobl-memcpy:cobl-string:cobl-vector:cobl-tree-map:cobl-strlen"

if [[ "$COMPILER_INFO" == *"clang"* ]]; then
  export ASAN_SYMBOLIZER_PATH=$(dirname $(which ${CC}))/llvm-symbolizer
fi
./build/test-coblang

# Run e2e tests over the examples.
python3 test-examples.py

# Run individual IR unittests.
python3 lit-tests.py -v tests
