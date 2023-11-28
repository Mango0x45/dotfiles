changequote([,])

define(__submap, [
	bind = $modkey, $2, submap, $1
	submap = $1
	$3
	bind = , escape, submap, reset
	submap = reset
])

define(__bindX, [
	ifelse([$#], 4, [
		bind$1 = $2, $3, exec, exec $4 2>/dev/null >&2
	], [
		bind$1 = $modkey, $2, exec, exec $3 2>/dev/null >&2
	])
])

define(__bind,  [__bindX(, $@)])
define(__binde, [__bindX(e,$@)])
define(__subbind, [
	__bindX(,,$@)
	bind = , $1, submap, reset
])
