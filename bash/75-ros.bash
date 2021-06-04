: ${ROS_DISTRO:?"variable is not set!"}

function image() { rosrun image_view image_view image:="$@"; }
function message() { rostopic echo -n 1 "$@"; }
