import os
import sys

PLOP_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "..")
print(f"Adding {PLOP_PATH} to sys.path")
sys.path.append(PLOP_PATH)

PUBLIC_PATH = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    "..",
    "..",
    "frontend",
    "public_generated",
)
os.makedirs(PUBLIC_PATH, exist_ok=True)
