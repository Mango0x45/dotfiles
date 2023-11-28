changequote([,])

define(__submap, [
	bind = $modkey, $2, submap, $1
	submap = $1
	$3
	bind = , escape, submap, reset
	submap = reset
])

define(__subbind, [
	bind = , $1, exec, exec $2 2>/dev/null
	bind = , $1, submap, reset
])
