#!/bin/sh

exe=$(find dist-newstyle -type f -executable)
version=$(git tag --list | head -n1)

if [ -z "$version" ]; then
  echo 'no git tags found'
  exit 1
fi

tar -czvf "$exe" emoji-cli_linux_x86_64-"$version".tar.gz
