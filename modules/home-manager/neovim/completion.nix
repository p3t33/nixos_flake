{ config, pkgs, lib, ... }:
let
  cfg = config.programs.neovim;
in
{
  config = lib.mkIf cfg.enable {
  programs.neovim = {

    extraPackages = with pkgs; [
      # LSP servers
      # ----------------
      lua-language-server # lua
      clang-tools # c/c++
      nixd # nix
      nixfmt-rfc-style # nix formator used by nixd
      jdt-language-server # java

      pyright # python
      # ----------------
    ];

    plugins = lib.mkOrder 400 (with pkgs.vimPlugins; [
      {
        plugin = lazydev-nvim;
        type = "lua";
        config = ''
          require("lazydev").setup({})
        '';
      }
      {
        # ==================================================
        # LSP configuration
        # ==================================================
        # This plugin is used for configuring and managing connections to language servers in Neovim.
        # While LSP support is indeed a part of Neovim's core, nvim-lspconfig is used to streamline and
        # customize the configuration for different language servers.
        #
        # It can work with language servers installed by Mason or with servers installed manually by
        # the user (e.g. using apt install or other package managers such as nix in this case).
        #
        # As I am using nix to intall my langage servers I don't need
        # to use mason to install them and mason-lspcofnig to allow nvim-lspconfig
        # to configre the langage servers installed by mason.
        #
        # This plugin "output" needs to be "piped" itno a completion framework
        # to be useful to the user.
        #
        # In this case it is "piped" by cmp-nvim-lsp to nvim-cmp completion
        # framework.
        plugin = nvim-lspconfig;
        type = "lua";
        config = ''
          local opts = { noremap = true, silent = true }

          local on_attach = function(client, bufnr)
            -- Enable completion triggered by <c-x><c-o>
            vim.bo[bufnr].omnifunc = 'v:lua.vim.lsp.omnifunc'

            local bufopts = { noremap=true, silent=true, buffer=bufnr }

            -- Keymaps
            vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
            vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
            vim.keymap.set("n", "<leader>vws", vim.lsp.buf.workspace_symbol, bufopts)
            vim.keymap.set("n", "<leader>vd", vim.diagnostic.open_float, bufopts)
            vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
            vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
            vim.keymap.set("n", "<leader>vca", vim.lsp.buf.code_action, bufopts)
            vim.keymap.set("n", "<leader>vrr", vim.lsp.buf.references, bufopts)
            vim.keymap.set("n", "<leader>vrn", vim.lsp.buf.rename, bufopts)
            vim.keymap.set("i", "<C-h>", vim.lsp.buf.signature_help, bufopts)
            vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, opts)
            vim.keymap.set('n', '<leader>f', function() vim.lsp.buf.format { async = true } end, bufopts)
          end

          local flags = { debounce_text_changes = 150 }
          local capabilities = require('cmp_nvim_lsp').default_capabilities()

          -- --- LSP SERVER CONFIGS ---
          vim.lsp.config.lua_ls = {
            on_attach = on_attach,
            flags = flags,
            capabilities = capabilities,
          }

          vim.lsp.config.clangd = {
            on_attach = on_attach,
            flags = flags,
            capabilities = capabilities,
            cmd = { "clangd", "--offset-encoding=utf-16" },
          }

          vim.lsp.config.pyright = {
            on_attach = on_attach,
            flags = flags,
            capabilities = capabilities,
          }

          vim.lsp.config.nixd = {
            on_attach = on_attach,
            flags = flags,
            capabilities = capabilities,
            settings = {
              nixd = { formatting = { command = { "nixfmt" } } }
            },
          }

          vim.lsp.config.jdtls = {
            on_attach = on_attach,
            flags = flags,
            capabilities = capabilities,
            cmd = {
              "jdtls",
              "-configuration", vim.fn.stdpath("cache") .. "/jdtls/config",
              "-data", vim.fn.stdpath("cache") .. "/jdtls/workspace",
            },
            root_markers = { "pom.xml", "gradle.build", ".git" },
          }

          -- --- ENABLE THEM ---
          vim.lsp.enable({
            "lua_ls",
            "clangd",
            "pyright",
            "nixd",
            "jdtls",
        })

        '';
      }

      # inputs/sources/adapters for nvim-cmp autocomplete framework
      # ==================================================

      # lsp source
      # ----------
      # "pipes" nvim-lspconfig into cmp-nvim
      cmp-nvim-lsp

      cmp-buffer
      cmp-path
      cmp-cmdline
      cmp-git

      # snippet source
      # --------------
      cmp_luasnip
      luasnip # snippet engine for cmp_luasnip
      friendly-snippets # snippet repository for luasnip

      # =======================================
      # Context aware auto completion framework
      # =======================================
      # nvim-cmp is a context aware completion framework that can be extened with other
      # plugins to be used as its input. Among them is cmp-nvim-lsp that provides
      # access to the language server that is configred with nvim-lspconfig.
      #
      # The "source" plugins for nvim-cmp added right above.
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
              luasnip.lsp_expand(args.body) -- For `luasnip` users.
              end
            },
            sources = {
              {name = 'nvim_lsp', keyword_length = 3},
              {name = 'buffer', keyword_length = 3},
              {name = 'path'},
              {name = 'luasnip', keyword_length = 2},
            },
            window = {
              documentation = cmp.config.window.bordered()
            },
            formatting = {
              fields = {'kind', 'abbr', 'menu'},
              format = function(entry, item)
              local menu_icon = {
                nvim_lsp = 'λ',
                luasnip = '',
                buffer = '󰂡',
                path = '',
              }

              item.menu = menu_icon[entry.source.name]
              return item
              end,
            },
            mapping = {
              ['<Up>'] = cmp.mapping.select_prev_item(select_opts),
              ['<Down>'] = cmp.mapping.select_next_item(select_opts),

              ['<C-k>'] = cmp.mapping.select_prev_item(select_opts),
              ['<C-j>'] = cmp.mapping.select_next_item(select_opts),

              ['<C-u>'] = cmp.mapping.scroll_docs(-4),
              ['<C-f>'] = cmp.mapping.scroll_docs(4),

              ['<C-e>'] = cmp.mapping.abort(),
              ['<CR>'] = cmp.mapping.confirm({select = true}), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.

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
                          if cmp.visible() then
                              cmp.select_next_item()
                          elseif luasnip.expand_or_jumpable() then
                              luasnip.expand_or_jump()
                          else
                              fallback()
                          end
                      end, { "i", "s" }),
              ['<S-Tab>'] = cmp.mapping(function(fallback)
                            if cmp.visible() then
                              cmp.select_prev_item()
                            elseif luasnip.jumpable(-1) then
                              luasnip.jump(-1)
                            else
                              fallback()
                            end
                          end, { "i", "s" }),
            },
          })

          cmp.setup.cmdline({ '/', '?' }, {
              mapping = cmp.mapping.preset.cmdline(),
              sources = {
                  { name = 'buffer' }  -- You can add more sources as needed
              }
          })

          cmp.setup.filetype('gitcommit', {
              sources = cmp.config.sources({
                  { name = 'git' },
              }, {
                  { name = 'buffer' },
              })
          })

          cmp.setup.cmdline(':', {
              mapping = cmp.mapping.preset.cmdline(),
              sources = cmp.config.sources({
                  { name = 'path' }
              }, {
                  { name = 'cmdline' }
              })
          })

        '';
      }
    ]);
  };
  };
}
