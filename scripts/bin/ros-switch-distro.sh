#!/bin/sh

sed 's/\(export ROS_DISTRO=\).*/\1'$1'/' .bashrc_local > .bashrc_local~
mv .bashrc_local~ .bashrc_local
