local luasnip = require('luasnip')
local lib = require('mango.lib')

lib.remap('i', '<C-l>', function() luasnip.jump(1) end)
lib.remap('i', '<C-h>', function() luasnip.jump(-1) end)
