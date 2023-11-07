local luasnip = require('luasnip')

vim.keymap.set('i', '<C-l>', function() luasnip.jump(1) end)
vim.keymap.set('i', '<C-h>', function() luasnip.jump(-1) end)
