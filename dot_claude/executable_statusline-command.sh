#!/bin/bash

input=$(cat)

model=$(echo "$input" | jq -r '.model.display_name // .model.id // "unknown"')
project_dir=$(echo "$input" | jq -r '.workspace.project_dir')
current_dir=$(echo "$input" | jq -r '.workspace.current_dir')

# Shorten paths
project_short="${project_dir/#$HOME/~}"
current_relative="${current_dir/#$project_dir\//}"

# Colors
BRIGHT_BLUE='\033[94m'
BOLD_WHITE='\033[1;97m'
NORMAL='\033[0m'
RED='\033[91m'
GREEN='\033[92m'

# Build output - start with project dir
if [ "$current_dir" != "$project_dir" ]; then
  output="${BOLD_WHITE}${project_short}${NORMAL} (${current_relative})"
else
  output="${BOLD_WHITE}${project_short}${NORMAL}"
fi

# Add model
output="$output  ${BRIGHT_BLUE}${model}${NORMAL}"

# Add git branch if in a repo
if git -C "$current_dir" rev-parse --git-dir > /dev/null 2>&1; then
  branch=$(git -C "$current_dir" symbolic-ref --short HEAD 2>/dev/null || git -C "$current_dir" rev-parse --short HEAD 2>/dev/null)
  if [ -n "$branch" ]; then
    # Check if dirty using status --porcelain
    if [ -n "$(git -C "$current_dir" status --porcelain 2>/dev/null)" ]; then
      output="$output  ${RED}${branch}${NORMAL}"
    else
      output="$output  ${GREEN}${branch}${NORMAL}"
    fi
  fi
fi

printf "%b" "$output"
