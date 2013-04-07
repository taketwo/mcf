export OGRE_RTT_MODE=Copy
export OGRE_RTT_MODE=FBO

function image() { rosrun image_view image_view image:="$@" ;}
function message() { rostopic echo -n 1 "$@" ;}

alias rosamke='rosmake'
