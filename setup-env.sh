CXX=${CXX:-clang++}
CC=${CC:-clang}
COBC=${COBC:-cobc}

COBC_BIN_DIR=$(dirname $(which ${COBC}))
COBC_ROOT_DIR=$(dirname ${COBC_BIN_DIR})
COBC_LIB_DIR=${COBC_ROOT_DIR}/lib
COBC_INCLUDE_DIR=${COBC_ROOT_DIR}/include

export LD_LIBRARY_PATH=${COBC_LIB_DIR}

COMPILER_INFO="$(${CC} --version)"
