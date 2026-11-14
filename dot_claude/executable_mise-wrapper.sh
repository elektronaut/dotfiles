#!/bin/bash
# Re-run under Homebrew's bash when the system one is older than 4.
if [ "${BASH_VERSINFO[0]}" -lt 4 ]; then
  for newer in /opt/homebrew/bin/bash /usr/local/bin/bash; do
    [ -x "$newer" ] && exec "$newer" "$0" "$@"
  done
fi

# Use DEBUG trap to ensure mise shims are first in PATH before each command,
# even after shell snapshot is sourced
mise_shims="$HOME/.local/share/mise/shims"
trap 'case ":$PATH" in ":$mise_shims:"*) ;; *) export PATH="$mise_shims:$PATH" ;; esac' DEBUG

# gh prefers GH_TOKEN over GITHUB_TOKEN. Claude Code injects a GitHub App user
# token (ghu_) into GH_TOKEN which cannot write pull requests, so prefer the
# PAT when one is present.
[[ -n $GITHUB_TOKEN ]] && export GH_TOKEN="$GITHUB_TOKEN"

eval "$@"
