import atexit
import os
import sys
from pathlib import Path
import readline


try:
	readline.parse_and_bind("tab: complete")
except ImportError:
	pass


if hasattr(sys, '__interactivehook__'):
	del sys.__interactivehook__


histfile = Path(os.getenv("XDG_CACHE_HOME", Path.home() / ".cache")) / "python_history"
try:
	histfile.touch(exist_ok=True)
except FileNotFoundError:
	histfile.parent.mkdir(parents=True, exist_ok=True)

readline.read_history_file(histfile)
readline.set_history_length(5_000)
atexit.register(readline.write_history_file, histfile)
