#!/usr/bin/env bash

# Packs a file or a directory in an archive of requested type
# Usage: pack TYPE FILE
# Supported archive types:
#     tbz
#     tgz
#     tar
#     bz2
#     gz
#     zip
#     7z
function pack()
{
  if [[ -f $2 || -d $2 ]]; then
    case $1 in
      tbz)      tar cjvf "$2.tar.bz2" "$2"                  ;;
      tgz)      tar czvf "$2.tar.gz" "$2"                   ;;
      tar)      tar cpvf "$2.tar" "$2"                      ;;
      bz2)      bzip2 "$2"                                  ;;
      gz)       gzip -c -9 -n "$2" > "$2.gz"                ;;
      zip)      zip -r "$2.zip" "$2"                        ;;
      7z)       7z a "$2.7z" "$2"                           ;;
      *)        echo "Do not know how to pack into '$1'..." ;;
    esac
  else
    echo "'$2' is not a valid file or directory!"
  fi
}
