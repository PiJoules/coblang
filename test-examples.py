import unittest
import os
import subprocess

from pathlib import Path


BUILD_DIR = Path("build")
STAGE1_BIN = BUILD_DIR / "coblang"
EXAMPLES_DIR = Path("examples")


class TestCompiler:
    def invoke(self, files):
        objfiles = []

        for filename, flags in files:
            objfile = str(BUILD_DIR / filename.name) + ".o"
            res = subprocess.run(
                [self.bin, str(filename), "-o", objfile] + flags, capture_output=True
            )
            self.assertEqual(res.returncode, 0)
            objfiles.append(objfile)

        exefile = str(BUILD_DIR / "a.out")
        res = subprocess.run(
            [os.environ.get("CC", "clang"), "-o", exefile] + objfiles,
            capture_output=True,
        )
        self.assertEqual(res.returncode, 0)

        res = subprocess.run([exefile], capture_output=True)
        self.assertEqual(res.returncode, 0)

        return res.stdout.decode("utf-8")

    def test_hello_world(self):
        self.assertEqual(
            self.invoke(
                [
                    (EXAMPLES_DIR / "hello-world.cbl", ["-x"]),
                ]
            ),
            "Hello world\n",
        )

    def test_hello_world_working_storage(self):
        self.assertEqual(
            self.invoke(
                [
                    (EXAMPLES_DIR / "hello-world-working-storage.cbl", ["-x"]),
                ]
            ),
            "Hello world    \n",
        )

    def test_calls(self):
        self.assertEqual(
            self.invoke(
                [
                    (EXAMPLES_DIR / "entry.cbl", []),
                    (EXAMPLES_DIR / "call-entry.cbl", ["-x"]),
                ]
            ),
            "Hello          \nworld!         \n",
        )


class TestStage1Compiler(unittest.TestCase, TestCompiler):
    def setUp(self):
        self.bin = STAGE1_BIN


if __name__ == "__main__":
    unittest.main()
