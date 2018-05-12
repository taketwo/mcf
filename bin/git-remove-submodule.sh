#!/bin/bash -e
## Remove the submodule
## Usage: git rm-submodule [ --no-commit ] dir
## does not commit if --no-commit is given 

## Script comes from https://github.com/kollerma/git-submodule-tools

## read input, display help if necessary
if [[ "$@" == "" || "$@" == *--help* ]]; then
    cat<<EOF
 Remove a submodule

 This command removes the submodule <directory> and creates a new commit
 with a simple commit message including the url of the removed
 submodule.

 Usage:
    git rm-submodule [--no-commit] <directory>

    --no-commit: do not commit the result
EOF
    exit 0;
fi

if [ "$1" == "--no-commit" ]; then
    nocommit=1
    dir=$2
else 
    dir=$1
fi

## check if this is a valid submodule
if [[ ! "$dir" || $(git ls-files --error-unmatch --stage -- "$dir" | grep -E '^160000') == "" ]]; then
    echo >&2 "Error: \"$dir\" is not a submodule."
    exit 1
fi

## get the full path of the submodule
dir=$(git ls-files --full-name "$dir")
## ensure that we are in the toplevel directory
cdup=$(git rev-parse --show-toplevel) &&
cd "$cdup" || {
    echo >&2 "Cannot chdir to $cdup, the toplevel of the working tree"
    exit 1
}

## seems we're safe, so start removing
## get submodule url
url=`git config --get submodule."$dir".url || echo "unknown"`
## remove config entries
git config -f .gitmodules --remove-section submodule."$dir" 2>/dev/null || echo -n ""
git config --remove-section submodule."$dir" 2>/dev/null || echo -n ""
git rm --cached "$dir"
rm -rf "$dir"
## commit changes
if [[ ! "$nocommit" ]]; then
    git add .gitmodules
    git commit -m "Removed submodule \"$dir\" (url: $url)"
fi
