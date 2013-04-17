#!/bin/sh

sed 's/\(export ROS_DISTRO=\).*/\1'$1'/' $HOME/.bashrc_local > $HOME/.bashrc_local~
mv $HOME/.bashrc_local~ $HOME/.bashrc_local
