set -e

source setup-env.sh

SRCDIR=$(pwd)

# This command gets the config directory from `cobc --info`.
# If we use a host `cobc`, then the cobc will just know which
# directory to use. But if we use one downloaded from sourceforge,
# the COB_CONFIG_DIR may point to a non-existant directory:
# something like `/usr/local/share/gnucobol/config`. If this is
# the case, try to find the directory using relative paths.
MAYBE_COBC_CONFIG_DIR=$(${COBC} --info | grep -e 'COB_CONFIG_DIR' | awk '{print $NF}')
if ! test -d ${MAYBE_COBC_CONFIG_DIR}; then
  export COB_CONFIG_DIR=${COBC_ROOT_DIR}/share/gnucobol/config
fi

COBL_FLAGS="-g --debug -Wall -Werror -I${SRCDIR} -fstatic-call"
COBL_FLAGS="${COBL_FLAGS} -I${COBC_INCLUDE_DIR}"

LLVM_CONFIG=${LLVM_CONFIG:-llvm-config}
LLVM_CONFIG_LD_FLAGS=$(${LLVM_CONFIG} --ldflags)
LLVM_CONFIG_SYSTEM_LIBS=$(${LLVM_CONFIG} --system-libs)
LLVM_CONFIG_CORE_LIBS=$(${LLVM_CONFIG} --libs core)

if [[ "$COMPILER_INFO" == *"clang"* ]]; then
  export COB_CC=${CC}
  export COB_CFLAGS="-fsanitize=address -Wno-deprecated-non-prototype -g3"
  export COB_LDFLAGS="-fsanitize=address"
fi

mkdir -p build
cd build
${COBC} ${COBL_FLAGS} -c -x ${SRCDIR}/coblang.cbl
${COBC} ${COBL_FLAGS} -c ${SRCDIR}/cobl-ctype.cbl
${COBC} ${COBL_FLAGS} -c ${SRCDIR}/cobl-memcpy.cbl
${COBC} ${COBL_FLAGS} -c ${SRCDIR}/lexer.cbl
${COBC} ${COBL_FLAGS} -c ${SRCDIR}/codegen.cbl
${COBC} ${COBL_FLAGS} -c ${SRCDIR}/cobl-string.cbl
${COBC} ${COBL_FLAGS} -c ${SRCDIR}/cobl-strlen.cbl
${COBC} ${COBL_FLAGS} -c ${SRCDIR}/cobl-utils.cbl
${COBC} ${COBL_FLAGS} -c ${SRCDIR}/cobl-vector.cbl
${COBC} ${COBL_FLAGS} -c ${SRCDIR}/cobl-tree-map.cbl
${COBC} ${COBL_FLAGS} -x \
  ${LLVM_CONFIG_CORE_LIBS} ${LLVM_CONFIG_LD_FLAGS} \
  ${LLVM_CONFIG_SYSTEM_LIBS} \
  -L${COBC_LIB_DIR} \
  -o coblang \
  coblang.o cobl-ctype.o cobl-utils.o cobl-vector.o \
  cobl-memcpy.o lexer.o codegen.o cobl-string.o \
  cobl-strlen.o cobl-tree-map.o

${COBC} ${COBL_FLAGS} -m cobl-string.o -L${COBC_LIB_DIR}
${COBC} ${COBL_FLAGS} -m cobl-memcpy.o -L${COBC_LIB_DIR}
${COBC} ${COBL_FLAGS} -m cobl-strlen.o -L${COBC_LIB_DIR}
${COBC} ${COBL_FLAGS} -m cobl-vector.o -L${COBC_LIB_DIR}
${COBC} ${COBL_FLAGS} -m cobl-tree-map.o -L${COBC_LIB_DIR}
${CXX} ${SRCDIR}/test-coblang.cpp -Wall -o test-coblang -I${GTEST_HDRS} -g3 \
  -L${GTEST_LIBS} -lgtest -lgtest_main -fuse-ld=lld `${COBC_BIN_DIR}/cob-config --cflags` \
  -L${COBC_LIB_DIR} -I${COBC_INCLUDE_DIR} \
  `${COBC_BIN_DIR}/cob-config --libs` -fsanitize=undefined -fsanitize=address -std=c++20 
