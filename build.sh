set -e

SRCDIR=$(pwd)
CXX=${CXX:-clang++}

COBL_FLAGS="-g --debug -Wall -Werror -I${SRCDIR} -fstatic-call"

LLVM_CONFIG=${LLVM_CONFIG:-llvm-config}
LLVM_CONFIG_LD_FLAGS=$(${LLVM_CONFIG} --ldflags)
LLVM_CONFIG_SYSTEM_LIBS=$(${LLVM_CONFIG} --system-libs)
LLVM_CONFIG_CORE_LIBS=$(${LLVM_CONFIG} --libs core)

mkdir -p build
cd build
cobc ${COBL_FLAGS} -c -x ${SRCDIR}/coblang.cbl
cobc ${COBL_FLAGS} -c ${SRCDIR}/external.cbl
cobc ${COBL_FLAGS} -c ${SRCDIR}/trimmed-string-length.cbl
cobc ${COBL_FLAGS} -c ${SRCDIR}/cobl-ctype.cbl
cobc ${COBL_FLAGS} -c ${SRCDIR}/cobl-getc.cbl
cobc ${COBL_FLAGS} -c ${SRCDIR}/cobl-io.cbl
cobc ${COBL_FLAGS} -c ${SRCDIR}/cobl-malloc.cbl
cobc ${COBL_FLAGS} -c ${SRCDIR}/cobl-memcpy.cbl
cobc ${COBL_FLAGS} -c ${SRCDIR}/lexer.cbl
cobc ${COBL_FLAGS} -c ${SRCDIR}/parser.cbl
cobc ${COBL_FLAGS} -c ${SRCDIR}/cobl-string.cbl
cobc ${COBL_FLAGS} -c ${SRCDIR}/cobl-utils.cbl
cobc ${COBL_FLAGS} -c ${SRCDIR}/cobl-vector.cbl
cobc ${COBL_FLAGS} -x \
  ${LLVM_CONFIG_CORE_LIBS} ${LLVM_CONFIG_LD_FLAGS} \
  ${LLVM_CONFIG_SYSTEM_LIBS} \
  -o coblang \
  coblang.o external.o trimmed-string-length.o \
  cobl-ctype.o cobl-getc.o cobl-io.o cobl-malloc.o \
  cobl-memcpy.o lexer.o parser.o cobl-string.o \
  cobl-utils.o cobl-vector.o

cobc ${COBL_FLAGS} -m cobl-string.o
cobc ${COBL_FLAGS} -m cobl-memcpy.o
cobc ${COBL_FLAGS} -m cobl-vector.o
${CXX} ${SRCDIR}/test-coblang.cpp -Wall -o test-coblang -I${GTEST_HDRS} \
  -L${GTEST_LIBS} -lgtest -lgtest_main -fuse-ld=lld `cob-config --cflags` \
  `cob-config --libs` -fsanitize=undefined -fsanitize=address -std=c++20 
