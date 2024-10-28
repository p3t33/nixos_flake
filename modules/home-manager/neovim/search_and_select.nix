{ pkgs, ... }:
{
  programs.neovim = {

    extraPackages = with pkgs; [
      # Dependencies
      # ----------------
      gdb
      lldb
    ];

    plugins = with pkgs.vimPlugins; [
      {
        plugin = fzf-lua;
        type = "lua";
        config = ''
          require('fzf-lua').setup({
             'default', -- the profile to be used.
             winopts = {
                 height = 0.90,  -- Adjust as needed
                 width = 0.90,   -- Adjust as needed
                 row = 0.50,     -- Center vertically
                 col = 0.50,     -- Center horizontally

                 preview = {
                     layout = "vertical",
                     vertical = "up:70%",
                     default="builtin"
                   },
             },

             fzf_opts = {
             ['--layout'] = 'reverse-list',
             },
          })

          vim.keymap.set('n', '<leader>ff', ':FzfLua files<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fb', ':FzfLua buffers<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fs', ':FzfLua live_grep<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fr', ':FzfLua resume<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fw', ':FzfLua grep_cword<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fW', ':FzfLua grep_cWORD<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fg', ':FzfLua git_bcommits<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fv', ':FzfLua grep_visual<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fc', ':FzfLua git_status<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fl', ':FzfLua lsp_finder<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fd', ':FzfLua diagnostics_workspace<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fh', ':FzfLua help_tags<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fts', ':FzfLua tags_live_grep<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>ftw', ':FzfLua tags_grep_cword<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>ftW', ':FzfLua tags_grep_cWORD<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>ftv', ':FzfLua tags_grep_visual<CR>', { noremap = true, silent = true })
        '';
      }

      # Telescope
      # ---------
      #
      # dependencies:
      telescope-vim-bookmarks-nvim
      telescope-ui-select-nvim
      {
        plugin = telescope-fzf-native-nvim;
        type = "lua";
        config = '''';
      }
      {
        plugin = telescope-nvim;
        type = "lua";
        config = ''
          local actions = require("telescope.actions")
          require('telescope').setup {
              defaults = {
                  mappings = {
                      i = {
                          ['<C-u>'] = false,
                          ['<C-d>'] = false,
                      },
                  },
                  dynamic_preview_title = true,
                  wrap_results = true,

                  layout_config = {
                      prompt_position = "top",
                      horizontal = {
                          preview_width = 0.65,
                          width = 0.75,
                      },
                      vertical = {
                          width = 0.9,  -- Adjusts the width of the Telescope window as a percentage of the total screen width
                          height = 0.95,  -- Adjusts the height as a percentage of the total screen height
                          mirror = false,  -- If true, flips the layout vertically (preview at bottom)
                          preview_height = 0.6,
                          preview_cutoff = 20,
                          prompt_position = "bottom",
                      },
                      center = {
                          mirror = false,
                          height = 0.40,  -- Height of the Telescope window as a percentage of the total screen height
                          width = 0.50,  -- Width of the Telescope window as a percentage of the total screen width
                          preview_cutoff = 40,  -- Minimum width of the Telescope window to show the preview pane
                      },
                      cursor = {
                          width = 0.80,  -- Width of the Telescope window as a percentage of the total screen width
                          height = 0.40,  -- Height of the Telescope window as a percentage of the total screen height
                          preview_cutoff = 40,  -- Minimum width of the Telescope window to show the preview pane
                      },
                  },
              },
              pickers = {
                  find_files = {
                      layout_strategy = 'vertical',
                  },
                  live_grep = {
                      layout_strategy = 'vertical',
                  },
                  grep_string = {
                      layout_strategy = 'vertical',
                  },
                  buffers = {
                      layout_strategy = 'vertical',
                  },
                  help_tags = {
                      layout_strategy = 'vertical',
                  },
                  git_status = {
                      layout_strategy = 'vertical',
                  },
              },
              extensions = {
                  -- telescope-fzf-native-nvim pluginreplaces native lua search
                  fzf = {
                      fuzzy = true,                    -- false will only do exact matching
                      override_generic_sorter = true,  -- override the generic sorter
                      override_file_sorter = true,     -- override the file sorter
                      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                  },
                  ["ui-select"] = {
                      require("telescope.themes").get_cursor(),
                  },
              }
          }

          -- for the use of telescope-fzf-native.nvim
          require('telescope').load_extension('fzf')
          -- for the use of telescope-vim-bookmarks-nvim
          require('telescope').load_extension('vim_bookmarks')
          require("telescope").load_extension("ui-select")

          local builtin = require('telescope.builtin')
          vim.keymap.set('n', '<leader>xf', builtin.find_files, {})
          vim.keymap.set('n', '<leader>xs', builtin.live_grep, {})
          vim.keymap.set('n', '<leader>xw', builtin.grep_string, {})
          vim.keymap.set('n', '<leader>xb', builtin.buffers, {})
          vim.keymap.set('n', '<leader>xh', builtin.help_tags, {})
          vim.keymap.set('n', '<leader>xc', builtin.git_status, {})
          vim.keymap.set('n', '<leader>xr', builtin.resume, {})
          vim.keymap.set('n', '<leader>fm', function()
              require('telescope').extensions.vim_bookmarks.all()
          end, {desc = 'Telescope: All Vim Bookmarks'})

          -- telescope unlike fzf-lua doesn't have lsp_code_actions picker
          -- Insted I am using a lower level function for telescope key bindings
          -- By using telescope-ui-select-nvim vim.ui.select will go to telescope.
          -- This way I am getting a "picker" that feels native to telescope.
          vim.keymap.set("n", "<leader>xd", vim.lsp.buf.code_action, bufopts)
        '';
      }
      # --------------------------------

      {
        # specify "hooks" on files and then manage them.
        plugin = harpoon2;
        type = "lua";
        config = ''
          local harpoon = require("harpoon")
          -- REQUIRED
          harpoon:setup()
          -- REQUIRED

          vim.keymap.set("n", "<leader>a", function() harpoon:list():add() end)
          vim.keymap.set("n", "<C-e>", function() harpoon.ui:toggle_quick_menu(harpoon:list()) end)

          vim.keymap.set("n", "<leader>1", function() harpoon:list():select(1) end)
          vim.keymap.set("n", "<leader>2", function() harpoon:list():select(2) end)
          vim.keymap.set("n", "<leader>3", function() harpoon:list():select(3) end)
          vim.keymap.set("n", "<leader>4", function() harpoon:list():select(4) end)
        '';
      }

    ];
  };
}
