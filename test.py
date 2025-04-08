import unittest
import os
import subprocess


STAGE1_BIN = "build/coblang"


class TestCompiler:
    def invoke(self, filename):
        res = subprocess.run([self.bin, filename], capture_output=True)
        self.assertEqual(res.returncode, 0)

        res = subprocess.run(
            [os.environ.get("CC", "clang"), "out.obj", "-o", "a.out"],
            capture_output=True,
        )
        self.assertEqual(res.returncode, 0)

        res = subprocess.run(["./a.out"], capture_output=True)
        self.assertEqual(res.returncode, 0)

        return res.stdout.decode("utf-8")

    def test_hello_world(self):
        self.assertEqual(self.invoke("hello-world.cbl"), "Hello world\n")


class TestStage1Compiler(unittest.TestCase, TestCompiler):
    def setUp(self):
        self.bin = STAGE1_BIN


if __name__ == "__main__":
    unittest.main()
