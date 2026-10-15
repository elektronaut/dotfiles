#!/bin/bash
# Use DEBUG trap to ensure mise shims are in PATH before each command,
# even after shell snapshot is sourced
trap 'export PATH="$HOME/.local/share/mise/shims:$PATH"' DEBUG

# gh prefers GH_TOKEN over GITHUB_TOKEN. Claude Code injects a GitHub App user
# token (ghu_) into GH_TOKEN which cannot write pull requests, so prefer the
# PAT when one is present.
[[ -n $GITHUB_TOKEN ]] && export GH_TOKEN="$GITHUB_TOKEN"

eval "$@"
