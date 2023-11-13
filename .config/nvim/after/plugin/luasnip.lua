local ls = require('luasnip')
local ts_utils = require('nvim-treesitter.ts_utils')
local ts_locals = require('nvim-treesitter.locals')

local c = ls.choice_node
local d = ls.dynamic_node
local f = ls.function_node
local i = ls.insert_node
local s = ls.s
local sn = ls.snippet_node
local t = ls.text_node
local fmt = require('luasnip.extras.fmt').fmt

ls.config.set_config {
	history = true,
	updateevents = 'TextChanged,TextChangedI',
}

ls.add_snippets('lua', {
	s('req', fmt([[local {} = require('{}')]], {
		f(function(name)
			local xs = vim.split(name[1][1], '.', { plain = true })
			return xs[#xs] or ''
		end, { 1 }),
		i(1),
	})),
})

local function is_ptr(str)
	return string.find(str, '*', 1, true) ~= nil
end

local function is_num(str)
	return vim.regex(
		'\\v^('
		.. 'u?int(8|16|32|64)?'
		.. '|byte'
		.. '|complex(64|128)'
		.. '|float(32|64)'
		.. '|rune'
		.. '|uintptr'
		.. ')$'
	):match_str(str) ~= nil
end

local transforms = {
	bool = 'false',
	string = '""',
	error = 'err',
	[is_num] = '0',
	[is_ptr] = 'nil',
}

local transform = function(text)
	local condition_matches = function(condition, ...)
		if type(condition) == 'string' then
			return condition == text
		else
			return condition(...)
		end
	end

	for condition, result in pairs(transforms) do
		if condition_matches(condition, text) then
			return t(result)
		end
	end

	return t(text .. '{}')
end

local handlers = {
	parameter_list = function(node)
		local result = {}

		local count = node:named_child_count()
		for idx = 0, count - 1 do
			local matching_node = node:named_child(idx)
			local type_node = matching_node:field('type')[1]
			table.insert(result, transform(
				vim.treesitter.get_node_text(type_node, 0)
			))
			if idx ~= count - 1 then
				table.insert(result, t({ ', ' }))
			end
		end

		return result
	end,

	type_identifier = function(node)
		local text = vim.treesitter.get_node_text(node, 0)
		return { transform(text) }
	end,
}

local function_node_types = {
	function_declaration = true,
	method_declaration = true,
	func_literal = true,
}

local function go_result_type()
	local cursor_node = ts_utils.get_node_at_cursor()
	if cursor_node == nil then
		print('Unable to find position')
		return t('')
	end
	local scope = ts_locals.get_scope_tree(cursor_node, 0)

	local function_node
	for _, v in ipairs(scope) do
		if function_node_types[v:type()] then
			function_node = v
			break
		end
	end

	if not function_node then
		print('Not inside of a function')
		return t('')
	end

	local query = vim.treesitter.query.parse('go', [[
		[
			(method_declaration result: (_) @id)
			(function_declaration result: (_) @id)
			(func_literal result: (_) @id)
		]
	]])
	for _, node in query:iter_captures(function_node, 0) do
		if handlers[node:type()] then
			return handlers[node:type()](node)
		end
	end
end

ls.add_snippets('go', {
	s(
		'f',
		fmt(
			[[
				func <><>() <>{
					<>
				}
			]],
			{
				c(1, {
					t(''),
					t('() '),
				}),
				i(2, 'name'),
				c(3, {
					t(''),
					t('() '),
				}),
				i(0),
			},
			{
				delimiters = '<>',
			}
		)
	),
	s(
		'ife',
		fmt(
			[[
				if err != nil {
					return <>
				}
			]],
			{
				d(1, function()
					return sn(nil, go_result_type())
				end),
			},
			{
				delimiters = '<>',
			}
		)
	),
})

vim.keymap.set({ 'i', 's' }, '<C-l>', function()
	if ls.expand_or_jumpable() then
		ls.expand_or_jump()
	end
end)
vim.keymap.set('i', '<C-h>', function()
	if ls.jumpable(-1) then
		ls.jump(-1)
	end
end)
vim.keymap.set('i', '<C-k>', function()
	if ls.choice_active() then
		ls.change_choice(-1)
	end
end)
vim.keymap.set('i', '<C-j>', function()
	if ls.choice_active() then
		ls.change_choice(1)
	end
end)
