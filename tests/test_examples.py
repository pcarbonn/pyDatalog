import subprocess
import sys
from pathlib import Path
import pytest

EXAMPLES_DIR = Path(__file__).parent.parent / "pyDatalog" / "examples"
EXAMPLE_FILES = [
    p for p in EXAMPLES_DIR.glob("*.py")
    if not p.name.startswith("test_") and not p.name.startswith("__")
]

# Check if pymongo is installed and MongoDB is available
has_mongo = False
try:
    from pymongo import MongoClient
    from pymongo.errors import ConnectionFailure
    try:
        # Try connecting with a very short timeout
        client = MongoClient("localhost", 27017, serverSelectionTimeoutMS=500)
        client.server_info()
        has_mongo = True
    except ConnectionFailure:
        pass
    except Exception:
        pass
except ImportError:
    pass

@pytest.mark.parametrize("example_path", EXAMPLE_FILES, ids=lambda p: p.name)
def test_example_execution(example_path):
    if example_path.name == "Mongo.py" and not has_mongo:
        pytest.skip("MongoDB is not running locally")

    # Run each example in a clean subprocess, setting cwd to the examples directory
    result = subprocess.run(
        [sys.executable, str(example_path)],
        capture_output=True,
        text=True,
        cwd=str(EXAMPLES_DIR)
    )
    assert result.returncode == 0, (
        f"Example {example_path.name} failed with exit code {result.returncode}.\n"
        f"STDOUT:\n{result.stdout}\n"
        f"STDERR:\n{result.stderr}"
    )
