: ${ROS_DISTRO:?"variable is not set!"}

ROS_WORKSPACE_SETUP="$ROS_WORKSPACE_PATH/devel/setup.bash"

if [ -f $ROS_WORKSPACE_SETUP ]; then
  source $ROS_WORKSPACE_SETUP
fi

export OGRE_RTT_MODE=Copy
export OGRE_RTT_MODE=FBO

function image() { rosrun image_view image_view image:="$@" ;}
function message() { rostopic echo -n 1 "$@" ;}

alias rosdr='rosrun rqt_reconfigure rqt_reconfigure'
alias rviz='rosrun rviz rviz'
