require('treesitter-context').setup {
	enable = true,
	max_lines = 2,
	multiline_threshold = 2,
	line_numbers = true,
	trim_scope = 'inner',
	mode = 'cursor',
}
