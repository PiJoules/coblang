set -e

# Run cobl-* unittests.
export COB_LIBRARY_PATH=build
export COB_PRE_LOAD="cobl-memcpy:cobl-string:cobl-vector"
./build/test-coblang

# Run e2e tests over the examples.
python3 test.py

# Run individual IR unittests.
python3 lit-tests.py -v tests
