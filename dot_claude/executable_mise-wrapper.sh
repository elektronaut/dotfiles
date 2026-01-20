#!/bin/bash
# Use DEBUG trap to ensure mise shims are in PATH before each command,
# even after shell snapshot is sourced
trap 'export PATH="$HOME/.local/share/mise/shims:$PATH"' DEBUG
eval "$@"
