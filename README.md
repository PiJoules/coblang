# [WIP] coblang - A COBOL frontend for LLVM

![Build Status w/Clang](https://github.com/PiJoules/coblang/actions/workflows/build.yml/badge.svg)

No one should use this.

## Deps 

- [LLVM v16.0.6](https://github.com/llvm/llvm-project/releases/tag/llvmorg-16.0.6)
- [GnuCOBOL v3.2.0](https://sourceforge.net/projects/gnucobol/files/gnucobol/3.2/gnucobol-3.2_bin.tar.xz/download)
- [GoogleTest v1.16.0](https://github.com/google/googletest/releases/tag/v1.16.0)

## Build

```sh
$ bash -x build.sh
```

Environment variables

- `CXX`: The C++ compiler to use for building the unittests. Defaults to `clang++`.
- `CC`: The C compiler to use for building examples in `test-examples.py`. Defaults to `clang`.
- `LLVM_CONFIG`: The `llvm-config` used for linking against LLVM libs. Defaults to `llvm-config`.
- `COBC`: The gnucobol binary to use for the initial coblang build. Defaults to `cobc`.
- `GTEST_HDRS`: The path to gtest headers.
- `GTEST_LIBS`: The path to gtest libraries.

## Test

```sh
$ bash -x run-tests.sh
```

Environment variables

### Reproducing the github actions builders

The github actions can also be tested locally via docker containers using
[act](https://github.com/nektos/act).

```
$ act --workflows .github/workflows/build.yml
```
