#!/bin/sh

exe=$(find dist-newstyle -type f -executable)
basename=$(basename "$exe")
version=$(git tag --list | head -n1)

if [ -z "$version" ]; then
  echo 'no git tags found'
  exit 1
fi

cp "$exe" "$basename"
tar -czvf emoji-cli_linux_x86_64-"$version".tar.gz "$basename"

rm "$basename"
