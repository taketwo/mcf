: ${ROS_DISTRO:?"variable is not set!"}

ROS_WORKSPACE_SETUP="$ROS_WORKSPACE_PATH/devel/setup.bash"

if [ -f $ROS_WORKSPACE_SETUP ]; then
  source $ROS_WORKSPACE_SETUP
fi

ROS_CATKIN_MAKE_REPLACEMENT="$ROS_WORKSPACE_PATH/catkin_make.sh"

if [ -f $ROS_CATKIN_MAKE_REPLACEMENT ]; then
  source $ROS_CATKIN_MAKE_REPLACEMENT
fi

export OGRE_RTT_MODE=Copy
export OGRE_RTT_MODE=FBO

function image() { rosrun image_view image_view image:="$@" ;}
function message() { rostopic echo -n 1 "$@" ;}

alias rosdr='rosrun rqt_reconfigure rqt_reconfigure'
alias rviz='rosrun rviz rviz'
