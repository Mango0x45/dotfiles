changequote([,])

define(__submap, [
	bind = $modkey, $2, submap, $1
	submap = $1
	$3
	bind = , catchall, submap, reset
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

define(__setup_env, [
	esyscmd([
		sed -E '
			/^export/!d
			/`/d
			s/^export ([A-Z0-9a-z_]+)="(.*)"/env = \1, \2/
			s/^export ([A-Z0-9a-z_]+)=\x27(.*)\x27/env = \1, \2/
			s/^export ([A-Z0-9a-z_]+)=(.*)/env = \1, \2/
		' ~/.bashrc
	])
])
