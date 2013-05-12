: ${ROS_DISTRO:?"variable is not set!"}

ROS_SETUP="/opt/ros/$ROS_DISTRO/setup.bash"

if [ -f $ROS_SETUP ]; then
  source $ROS_SETUP
fi

export OGRE_RTT_MODE=Copy
export OGRE_RTT_MODE=FBO

function image() { rosrun image_view image_view image:="$@" ;}
function message() { rostopic echo -n 1 "$@" ;}

alias rosdr='rosrun dynamic_reconfigure reconfigure_gui'
alias rviz='rosrun rviz rviz'

# correct usual misspelling
alias rosamke='rosmake'
