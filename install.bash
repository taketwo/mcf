#!/bin/bash

echo "Creating symlinks for the config files in MCF..."

echo "[1] Remap CapsLock to Escape"
ln -s .mcf/.Xmodmap ~/.Xmodmap
echo "[2] Enable Solarized colors in terminal"
bash install-solarized-colors.bash
ln -s .mcf/.dircolors ~/.dircolors
ln -s .mcf/.fonts ~/.fonts
ln -s .mcf/.vim ~/.vim
ln -s .mcf/.vimrc ~/.vimrc
ln -s .mcf/.bashrc ~/.bashrc
ln -s .mcf/.gitconfig ~/.gitconfig

# Clang installation.
# 1) Follow instructions at http://clang.llvm.org/get_started.html
# 2) Configure with options:
#    ../configure --enable-optimized --disable-assertions --prefix=/opt/llvm

echo "Done"
