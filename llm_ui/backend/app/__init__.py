import os, sys

PLOP_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "..")
sys.path.append(PLOP_PATH)

PUBLIC_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "frontend", "public_generated")
os.makedirs(PUBLIC_PATH, exist_ok=True)