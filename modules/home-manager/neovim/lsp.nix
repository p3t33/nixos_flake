{ pkgs, ... }: {
    programs.neovim.plugins = with pkgs.vimPlugins; [
    # Code completion and LSP
    # =======================
    {
        # This plugin is the base for all of the LSP functionality.
        # It can work just fine without nvim-cmp but not the
        # other way around. But if nvim-cmp isn't installed and
        # set the amount of default information will be very
        # minimal.
        plugin = nvim-lspconfig;
        type = "lua";
        config = ''
            local opts = { noremap=true, silent=true }

            -- Use an on_attach function to only map the following keys
            -- after the language server attaches to the current buffer
            local on_attach = function(client, bufnr)

            -- Enable completion triggered by <c-x><c-o>
            vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

            -- Mappings.
            -- See `:help vim.lsp.*` for documentation on any of the below functions
            local bufopts = { noremap=true, silent=true, buffer=bufnr }

            -- go to definition.
            vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
            vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
            vim.keymap.set("n", "<leader>vws", vim.lsp.buf.workspace_symbol, bufopts)
            vim.keymap.set("n", "<leader>vd", vim.diagnostic.open_float, bufopts)
            vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
            vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
            -- clang-tidy code actions
            vim.keymap.set("n", "<leader>vca", vim.lsp.buf.code_action, bufopts)
            vim.keymap.set("n", "<leader>vrr", vim.lsp.buf.references, bufopts)
            vim.keymap.set("n", "<leader>vrn", vim.lsp.buf.rename, bufopts)
            vim.keymap.set("i", "<C-h>", vim.lsp.buf.signature_help, bufopts)

            -- Can be used to replace plugin trouble-nvim(and dependencie nvim-web-devicons)
            vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, opts)

            -- clang format.
            vim.keymap.set('n', '<leader>f', function() vim.lsp.buf.format { async = true } end, bufopts)
            end

            local lsp_flags = {
                -- This is the default in Nvim 0.7+
                    debounce_text_changes = 150,
            }
          require('lspconfig').rust_analyzer.setup{}
          require('lspconfig').rnix.setup{}
          require('lspconfig').lua_ls.setup{}
          require('lspconfig').clangd.setup{
              on_attach = on_attach,
              flags = lsp_flags,
          }

          require('lspconfig').pyright.setup{
              on_attach = on_attach,
              flags = lsp_flags,
          }
        '';
      }

            # LSP completion related capabilities
      # -----------------------------------
      # This plugin is responsible for code completion,
      # code snippets(for function), and errors with explanations.
      # This plugin is depended on nvim-lspconfig plugin to have minimal
      # functionality. It needs bunch of other plugins(added right below)
      # to function properly.
      {
        plugin = nvim-cmp;
        type = "lua";
        config = ''
        vim.opt.completeopt = {'menu', 'menuone', 'noselect'}

        require('luasnip.loaders.from_vscode').lazy_load()

        local cmp = require('cmp')
        local luasnip = require('luasnip')

        local select_opts = {behavior = cmp.SelectBehavior.Select}

        cmp.setup({
          snippet = {
            expand = function(args)
            luasnip.lsp_expand(args.body)
            end
          },
          sources = {
            {name = 'path'},
            {name = 'nvim_lsp', keyword_length = 3},
            {name = 'buffer', keyword_length = 3},
            {name = 'luasnip', keyword_length = 2},
          },
          window = {
            documentation = cmp.config.window.bordered()
          },
          formatting = {
            fields = {'menu', 'abbr', 'kind'},
            format = function(entry, item)
            local menu_icon = {
              nvim_lsp = 'λ',
              luasnip = '⋗',
              buffer = 'Ω',
              path = '🖫',
            }

            item.menu = menu_icon[entry.source.name]
            return item
            end,
          },
          mapping = {
            ['<Up>'] = cmp.mapping.select_prev_item(select_opts),
            ['<Down>'] = cmp.mapping.select_next_item(select_opts),

            ['<C-p>'] = cmp.mapping.select_prev_item(select_opts),
            ['<C-n>'] = cmp.mapping.select_next_item(select_opts),

            ['<C-u>'] = cmp.mapping.scroll_docs(-4),
            ['<C-f>'] = cmp.mapping.scroll_docs(4),

            ['<C-e>'] = cmp.mapping.abort(),
            ['<CR>'] = cmp.mapping.confirm({select = false}),

            ['<C-d>'] = cmp.mapping(function(fallback)
            if luasnip.jumpable(1) then
            luasnip.jump(1)
            else
            fallback()
            end
            end, {'i', 's'}),

            ['<C-b>'] = cmp.mapping(function(fallback)
            if luasnip.jumpable(-1) then
            luasnip.jump(-1)
            else
            fallback()
            end
            end, {'i', 's'}),

            ['<Tab>'] = cmp.mapping(function(fallback)
            local col = vim.fn.col('.') - 1

            if cmp.visible() then
            cmp.select_next_item(select_opts)
            elseif col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
            fallback()
            else
            cmp.complete()
            end
            end, {'i', 's'}),

            ['<S-Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
            cmp.select_prev_item(select_opts)
            else
            fallback()
            end
            end, {'i', 's'}),
          },
        })
        '';
      }

      # nvim-cmp dependencies
      # ---------------------
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      cmp-cmdline

      # users of luasnip
      luasnip
      cmp_luasnip

      # This one wasn't on the list of nvim-cmp but in a reference
      # and is used in require('luasnip.loaders.from_vscode').lazy_load().
      friendly-snippets
  ];
}
