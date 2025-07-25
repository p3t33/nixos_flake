{ pkgs, lib, config, ... }:
let
  cfg = config.programs.neovim.enable;
  spelunk-nvim = pkgs.vimUtils.buildVimPlugin {
    pname = "spelunk.nvim";
    version = "unstable-2025-01-15";
    src = pkgs.fetchgit {
      url = "https://github.com/EvWilson/spelunk.nvim.git";
      rev = "2ef99a8c9ba50a9e85b68022c31f028faf538fe3";
      sha256 = "1nwm5fff4f5mpmzm33b14pgvbjmvbjf7l2db6ad6p1nj7bnwd2gg";
    };
  };
in
{
  config = lib.mkIf cfg {
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
                     -- sets the preview to use the plugin's own built in previewer.
                     default="builtin"
                   },
             },

             keymap = {
               -- keys for the built in previewer.
               builtin = {
                 ["<C-j>"] = "preview-down",
                 ["<C-k>"] = "preview-up",
               },
             },


             fzf_opts = {
             ['--layout'] = 'reverse-list',
             },

             oldfiles = {
               include_current_session = true,
               cwd_only = true,
               stat_file = true, -- verify files exist on disk
             },

             grep = {
               rg_glob        = true,          -- turn prompt-side globbing ON
               glob_flag      = "--iglob",     -- case-insensitive glob for RG
               glob_separator = "%s%-%-",      -- space + two dashes delimiter
               actions = {                     -- optional quality-of-life
                 ["ctrl-g"] = require("fzf-lua.actions").toggle_ignore, -- toggle .gitignore
             },
            },
          })

          vim.keymap.set('n', '<leader>fr', ':FzfLua resume<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fx', ':FzfLua search_history<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>ff', ':FzfLua files<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fb', ':FzfLua buffers<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fs', ':FzfLua live_grep<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fv', ':FzfLua grep_visual<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fw', ':FzfLua grep_cword<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fW', ':FzfLua grep_cWORD<CR>', { noremap = true, silent = true })
          vim.keymap.set('n', '<leader>fg', ':FzfLua git_bcommits<CR>', { noremap = true, silent = true })
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
      telescope-fzf-native-nvim
      smart-open-nvim
      sqlite-lua # reuired by smart-open-nvim
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
                          ["<C-k>"] = actions.preview_scrolling_up,
                          ["<C-j>"] = actions.preview_scrolling_down,
                      },
                  },
                  dynamic_preview_title = true,
                  wrap_results = true,

                  layout_config = {
                      prompt_position = "top",
                      horizontal = {
                          preview_width = 0.65,
                          width = 0.75,
                          scroll_speed = 0.1,
                      },
                      vertical = {
                          width = 0.9,  -- Adjusts the width of the Telescope window as a percentage of the total screen width
                          height = 0.95,  -- Adjusts the height as a percentage of the total screen height
                          mirror = false,  -- If true, flips the layout vertically (preview at bottom)
                          preview_height = 0.6,
                          preview_cutoff = 20,
                          prompt_position = "bottom",
                          scroll_speed = 0.1,
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
                  smart_open = {
                      match_algorithm = 'fzf',
                      disable_devicons = false,
                      cwd_only = true,
                      show_scores = true,
                      formatter = function(file)
                    return file.path
                    end,
                  },
              }
          }

          -- for the use of telescope-fzf-native.nvim
          require('telescope').load_extension('fzf')
          -- for the use of telescope-vim-bookmarks-nvim
          require('telescope').load_extension('vim_bookmarks')
          require('telescope').load_extension('smart_open')
          require("telescope").load_extension("ui-select")

          local builtin = require('telescope.builtin')
          vim.keymap.set('n', '<leader>xr', builtin.resume, {})
          vim.keymap.set('n', '<leader>xf', builtin.find_files, {})
          vim.keymap.set('n', '<leader>xb', builtin.buffers, {})
          vim.keymap.set('n', '<leader>xs', builtin.live_grep, {})
          vim.keymap.set('n', '<leader>xw', builtin.grep_string, {})
          vim.keymap.set('n', '<leader>xg', builtin.git_bcommits, {})
          vim.keymap.set('n', '<leader>xc', builtin.git_status, {})
          vim.keymap.set('n', '<leader>xh', builtin.help_tags, {})
          vim.keymap.set('n', '<leader>fm', function()
          -- As an extention ignores globaly layout config.
          require('telescope').extensions.vim_bookmarks.all({
              layout_strategy = 'vertical',
              layout_config = {
              prompt_position = "bottom",
              preview_height = 0.6,
              width = 0.9,
              height = 0.95,
              }
              })
          end, {desc = 'Telescope: All Vim Bookmarks'})


          -- telescope unlike fzf-lua doesn't have lsp_code_actions picker
          -- Insted I am using a lower level function for telescope key bindings
          -- By using telescope-ui-select-nvim vim.ui.select will go to telescope.
          -- This way I am getting a "picker" that feels native to telescope.
          vim.keymap.set("n", "<leader>xd", vim.lsp.buf.code_action, bufopts)

          -- As an extention ignores globaly layout config.
          vim.keymap.set('n', '<leader><leader>', function()
          require('telescope').extensions.smart_open.smart_open({
            layout_strategy = 'vertical',
              layout_config = {
              prompt_position = "bottom",
              preview_height = 0.6,
              width = 0.9,
              height = 0.95,
            }
          })
          end, {desc = 'Telescope: Smart Open'})
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
      {
        plugin = arrow-nvim;
        type = "lua";
        config = ''
          require('arrow').setup({
            show_icons = true,
            leader_key = ';', -- Recommended to be a single key
            buffer_leader_key = 'm', -- Per Buffer Mappings
            separate_by_branch = false,
            global_bookmarks = false,
            window = {
              width = "auto",
              height = "auto",
              border = "rounded",
            },
            per_buffer_config = {
              lines = 4,
              sort_automatically = true,
            },
          })

          -- Quick access keymaps
          vim.keymap.set("n", "H", require("arrow.persist").previous)
          vim.keymap.set("n", "L", require("arrow.persist").next)
          -- vim.keymap.set("n", "<C-s>", require("arrow.persist").toggle)
        '';
      }
    ];
  };
  };
}
