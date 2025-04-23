import lit.formats
import os
from pathlib import Path

BUILD_DIR = Path("build")
STAGE1_BIN = BUILD_DIR / "coblang"
FILECHECK = os.environ.get("FILECHECK", "FileCheck")

config.name = "My Example"
config.test_format = lit.formats.ShTest(True)
config.substitutions.append(("coblang", str(STAGE1_BIN.absolute())))
config.substitutions.append(("FileCheck", FILECHECK))
config.test_exec_root = str(BUILD_DIR.absolute())

config.suffixes = [".cbl"]
