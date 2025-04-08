set -e

export COB_LIBRARY_PATH=build
export COB_PRE_LOAD="cobl-memcpy:cobl-string:cobl-vector"
./build/test-coblang

python3 test.py
