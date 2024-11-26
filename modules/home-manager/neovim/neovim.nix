{ pkgs, ... }:

# As the order in which settings are being evaluate by neovim is important there
# is a need to understand how nix generates the configurations for neovim.
#
# There are 4 types of configurations
# - extraConfig: which is general vim script.
# - extraLuaConfig: which is general lua configuration.
# - plugin config: which can be of type lua or vim script.
#
# What is generated
# - init.lua file which is located at ~/.config/nvim/init.lua
# - vim script config file that is not located directly inside of ~/.config/nvim
# but is also part of the /nix/store.
#
# init.lua is sourcing the vim script config at the very top. Meaning that
# typescript configuration will be evaluated by neovim first and only then lua
# configuration.
#
# How are the configuration files being constructed?
# - for lua, everything in extraLuaConfig goes to the very top, then all the
# configuration provided by the plugins from type lua.
# - for vim type script everything in extraConfig goes to the very top then all the configuration
# provided by the plugins that are not defined as lua.
#
#
# There are two degrees of freedom when it comes to configuration(both plugins
# and configuration in extraCofnig/extraLuaConfig).
# - The order of imports at the top of the file.
# - The order of plugins.
#
# Meaning:
# - The first plugin in the first import will be the first
# plugin configuration to be generated right after the configuration in
# extraLuaConfig/extraConfig.
# - The first extraConfig/extraLuaConfig in the first import will be at the very
# top of init.lua or the vim typescript files.

{
  imports = [
    ./ui.nix
    ./completion.nix
    ./debugger.nix
    ./search_and_select.nix
    ./firefox_integration.nix
    ./spelling.nix
  ];

  programs.neovim = {
    enable = true;
    vimAlias = true;

    # Dependency management
    extraPackages = with pkgs; [
      # required by neovim/vim for copy/paste
      # to work with system clipboard on x11.
      xclip
    ];

    plugins = with pkgs.vimPlugins; [

      #
      # Utils
      # ========================

      # This is a dependence for many other plugins and could be considered as
      # a "library"
      plenary-nvim
      {
        # vim-tmux-navigator plugin has a dual function(although name implies only
        # vim integration )
        #
        # This plugin adds smart movements between vim windows and tmux panes, By using
        # Ctrl+h/j/k/l you will be able to move across tmux pane into pane with
        # vim inside it and then move inside the vim windows and back seamlessly
        # For this integration to work counterpart plugin needs to be added to tmux.
        #
        # If the counterpart isn't installed the only functionality that will be added
        # is the ability to move between windows using Ctr+h/j/k/l in vim.
        plugin = vim-tmux-navigator;
      }

      {
        plugin = nvim-surround;
        type = "lua";
        config = ''
          require("nvim-surround").setup ({
          })
        '';
      }
      {
        # A tree file explore.
        plugin = nvim-tree-lua;
        type = "lua";
        config = ''
          require'nvim-tree'.setup {}
        '';
      }
      {
        # a vim bookmarks manager
        plugin = vim-bookmarks;
        config = ''
          let g:bookmark_save_per_working_dir = 1
          let g:bookmark_no_default_key_mappings = 1

          "I need to define leader key here because the order in which "
          "nix generates the config"
          let mapleader = " "
          nnoremap <leader>mm :BookmarkToggle<CR>
          nnoremap <leader>mi :BookmarkAnnotate<CR>
          nnoremap <leader>ma :BookmarkShowAll<CR>
          nnoremap <leader>md :BookmarkClearAll<CR>
          nnoremap <leader>mn :BookmarkNext<CR>
          nnoremap <leader>mp :BookmarkPrev<CR>
        '';
      }
      # Tree-sitter
      # ----------
      # Tree-sitter is a parsing library for programming languages that
      # can be used to analyze, edit, and transform code.
      # This setting replaced:
      #
      # "ensure_installed = { "c", "cpp"..}" that was part
      # of the config = ''... and made vim install treesitter
      # packages dynamically instaed of using the declarative
      # nature of nix.
      #
      # By using withAllGrammars I am installing all of the packages
      # that can are avaliable to treesitter. This single setting
      # replaced.
      # plugin = (nvim-treesitter.withPlugins (p: [
      #     p.tree-sitter-nix
      #     p.tree-sitter-bash
      #     ....
      #     ]));

      {
        plugin = nvim-treesitter.withAllGrammars;
        type = "lua";
        config = ''
          require('nvim-treesitter.configs').setup {
          -- depending on nvim-treesitter-textobjects pluging
          textobjects = {
              lsp_interop = {
                  enable = true,
                  border = 'none',
                  floating_preview_opts = {},
                  peek_definition_code = {
                      ["<leader>df"] = "@function.outer",
                      ["<leader>dF"] = "@class.outer",
                  },
              },

              select = {
                  enable = true,

                  -- Automatically jump forward to textobj, similar to targets.vim
                      lookahead = true,

                  keymaps = {
                      ['aa'] = '@parameter.outer',
                      ['ia'] = '@parameter.inner',
                      ['af'] = '@function.outer',
                      ['if'] = '@function.inner',
                      ['ac'] = '@class.outer',
                      ['ic'] = '@class.inner',
                      ['ii'] = '@conditional.inner',
                      ['ai'] = '@conditional.outer',
                      ['il'] = '@loop.inner',
                      ['al'] = '@loop.outer',
                      ['at'] = '@comment.outer',
                  },
                  -- You can choose the select mode (default is charwise 'v')
                      --
                      -- Can also be a function which gets passed a table with the keys
                      -- * query_string: eg '@function.inner'
                      -- * method: eg 'v' or 'o'
                      -- and should return the mode ('v', 'V', or '<c-v>') or a table
                      -- mapping query_strings to modes.
                      selection_modes = {
                          ['@parameter.outer'] = 'v', -- charwise
                              ['@function.outer'] = 'V', -- linewise
                                  ['@class.outer'] = '<c-v>', -- blockwise
                      },
                  -- If you set this to `true` (default is `false`) then any textobject is
                      -- extended to include preceding or succeeding whitespace. Succeeding
                      -- whitespace has priority in order to act similarly to eg the built-in
                      -- `ap`.
                      --
                      -- Can also be a function which gets passed a table with the keys
                      -- * query_string: eg '@function.inner'
                      -- * selection_mode: eg 'v'
                      -- and should return true of false
                      include_surrounding_whitespace = true,
              },
          },

           highlight = {
             enable = true,
             additional_vim_regex_highlighting = {'org'},
           },
          }
        '';
      }
      # Depended on nvim-treesitter
      # and provides syntax aware text-objects, select, move, swap, and peek support.
      nvim-treesitter-textobjects
      # --------------------------------

      {
        # Undotree visualizes the undo history and makes it easy to
        # browse and switch between different undo branches.
        plugin = undotree;
        config = ''
          "I need to define leader key here because the order in which "
          "nix generates the config"
          let mapleader = " "
          nnoremap <leader>u :UndotreeToggle<CR>

          if has("persistent_undo")
              " Let's save undo info!
              if !isdirectory($HOME."/.vim")
                  call mkdir($HOME."/.vim", "", 0770)
              endif

              set undodir=~/.vim/undo-dir
              set undofile
          endif
          "---------------------------"
        '';
      }
      # ========================

      #
      # git
      # ========================
      {
        plugin = gitsigns-nvim;
        type = "lua";
        config = ''
          require('gitsigns').setup {
              on_attach = function(bufnr)
              local gs = package.loaded.gitsigns
                  vim.keymap.set('n', "<leader>gd", gs.reset_hunk)
                  vim.keymap.set('v', "<leader>gd", function() gs.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
                  vim.keymap.set('n', "<leader>gb", gs.toggle_current_line_blame)
                  vim.keymap.set('n', "]g", function()
                          if vim.wo.diff then return ']c' end
                          vim.schedule(function() gs.next_hunk() end)
                          return '<Ignore>'
                          end, {expr=true})
                  vim.keymap.set('n', "[g", function()
                          if vim.wo.diff then return '[c' end
                          vim.schedule(function() gs.prev_hunk() end)
                          return '<Ignore>'
                          end, {expr=true})
          end
          }
        '';
      }
      # ========================

      #
      # Miscellaneous
      # =================================
      {
        plugin = nvim-autopairs;
        type = "lua";
        config = ''
          require("nvim-autopairs").setup {}
        '';
      }
      {
        plugin = comment-nvim;
        type = "lua";
        config = ''
          require('Comment').setup ({
              toggler = {
                  line = '<leader><tab><tab>'
              },

              opleader = {
                  line = '<leader><tab><tab>'
              },

              extra = {
                  ---Add comment on the line above
                      above = '<leader><tab>O',
                  ---Add comment on the line below
                      below = '<leader><tab>o',
                  ---Add comment at the end of line
                      eol = '<leader><tab>A',
              },
          })
        '';
      }

      # Automatic closing of quotes, parenthesis, brackets...
      delimitMate

      # org-mode support
      {
        plugin = orgmode;
        type = "lua";
        config = ''
          -- Orgmode setup
          require('orgmode').setup({
                  org_agenda_files = {'~/Dropbox/org/*', '~/my-orgs/**/*'},
                  org_default_notes_file = '~/Dropbox/org/refile.org',
                  });
        '';
      }
      {
        plugin = vim-suda;
        config = ''
          cnoremap w!! :SudaWrite<CR>
        '';
      }
    ];

    extraLuaConfig = ''
          -- ============
          -- Editor setup
          -- ============
          vim.g.mapleader = " "

          -- No timeout for leader key
          vim.o.timeout = false
          vim.o.ttimeout = false

          -- Colorscheme
          vim.cmd("colorscheme codedark")

          -- Line numbers and relative lines
          vim.wo.number = true
          vim.wo.relativenumber = true

          -- Indentation settings of 4 spaces
          vim.o.tabstop = 4
          vim.o.softtabstop = 4
          vim.o.shiftwidth = 4
          vim.o.expandtab = true

          -- Use file type based indentation
          vim.cmd("filetype plugin indent on")

          -- Use cindent for C/C++
          vim.api.nvim_create_autocmd("FileType", {
                  pattern = {"c", "cpp"},
                  callback = function()
                  vim.bo.cindent = true
                  end,
                  })

          -- Set cursorline
          vim.wo.cursorline = true

          -- Disable wrap of text
          vim.wo.wrap = false

          -- incremental search, tarts searching as you type, immediately jumps to the closest match to the text you've entered so far
          vim.o.incsearch = true

          -- Enable true color support in the terminal
          vim.o.termguicolors = true

          -- set the number of lines to keep above and below the cursor when scrolling through a document
          vim.o.scrolloff = 8

          -- A column that is always displayed
          -- A plugin is used to populate it with data
          -- E.g erros, git information
          vim.o.signcolumn = "yes"

          -- Set update time for various features like diagnostics to appear
          vim.o.updatetime = 50

          -- Creates a ruler at 80 characters width
          vim.wo.colorcolumn = "80"

          -- Enable mouse support in all modes
          vim.o.mouse = "a"

          -- ------swap/backup file disable---
          -- As I am using undotree I don't need this file
          vim.o.swapfile = false
          vim.o.backup = false
          -- ---------------------------------

         -- ================================
         -- Costum functions
         -- ================================

         -- ----------------------------------------------------------
         -- Remove white spaces at the end of the line on buffer write
         -- ----------------------------------------------------------
         -- Define the Lua function to trim whitespace
         local function trim_whitespace()
              local save = vim.fn.winsaveview()
              vim.api.nvim_exec('%s/\\s\\+$//e', false)
              vim.fn.winrestview(save)
         end

         -- Set up an autocmd to call the Lua function before buffer write
         vim.api.nvim_create_autocmd("BufWritePre", {
                 pattern = "*",
                 callback = trim_whitespace,
         })


         -- ================================

         -- ================================
         -- Key Mappings
         -- ================================
         -- General options for mappings
         local opts = { noremap = true, silent = true }

        -- Drag up and down selected lines in visual line mode
        vim.keymap.set('v', 'J', ":m '>+1<CR>gv=gv", opts)
        vim.keymap.set('v', 'K', ":m '<-2<CR>gv=gv", opts)

        -- Centered movements
        -- ------------------
        vim.keymap.set('n', '<C-u>', "<C-u>zz", opts)
        vim.keymap.set('n', '<C-d>', "<C-d>zz", opts)
        vim.keymap.set('n', 'n', "nzzzv", opts)
        vim.keymap.set('n', 'N', "Nzzzv", opts)
        vim.keymap.set('n', '{', '{zz', opts)
        vim.keymap.set('n', '}', '}zz', opts)
        vim.keymap.set('n', '<C-i>', '<C-i>zz', opts)
        vim.keymap.set('n', '<C-o>', '<C-o>zz', opts)
        vim.keymap.set('n', '%', '%zz', opts)
        vim.keymap.set('n', '*', '*zz', opts)
        vim.keymap.set('n', '#', '#zz', opts)

       -- Paste over and the deleted text will go into void register
       vim.keymap.set('x', '<leader>p', '"_dP', opts)

       -- Deleting into void register
       vim.keymap.set('n', '<leader>d', '"_d', opts)
       vim.keymap.set('v', '<leader>d', '"_d', opts)

       -- Yanking into clipboard
       vim.keymap.set('n', '<leader>y', '"+y', opts)
       vim.keymap.set('v', '<leader>y', '"+y', opts)
       vim.keymap.set('n', '<leader>Y', '"+Y', opts)

       -- Column mode edit exit
       vim.keymap.set('i', '<C-c>', '<Esc>', opts)

       -- Replace the word you are on
       vim.keymap.set('n', '<leader>s', ":%s/\\<<C-r><C-w>\\>/<C-r><C-w>/gI<Left><Left><Left>", opts)

       -- Make file executable
       vim.keymap.set('n', '<leader>x', "<cmd>!chmod +x %<CR>", opts)

       -- NvimTree toggle
       vim.keymap.set('n', '<leader>.', ":NvimTreeToggle<Cr>", opts)


       -- Save current buffer
       vim.keymap.set('n', '<C-s>', ":w<CR>", opts)
       vim.keymap.set('i', '<C-s>', "<Esc>:w<CR>", opts)

       -- Save and exit file
       vim.keymap.set('n', '<C-x>', ":wq<CR>", opts)
       vim.keymap.set('i', '<C-x>', "<Esc>:wq<CR>", opts)

       -- Exit vim wihtout saving
       vim.keymap.set('n', '<C-q>', ":q!<CR>", opts)
       vim.keymap.set('i', '<C-q>', "<Esc>:q!<CR>", opts)

       -- Generate ctags
       vim.keymap.set('n', '<leader>gt', ":!ctags -R --exclude=.git<Cr><Cr>", opts)

       -- Bookmarks (Add your bookmark functions in a similar way)

       -- Buffer control
       vim.keymap.set('n', '<leader>bn', ":bn<Cr>", opts)
       vim.keymap.set('n', '<leader>bp', ":bp<Cr>", opts)
       vim.keymap.set('n', '<leader>bd', ":bd<Cr>", opts)
       vim.keymap.set('n', '<leader>bs', "<C-w>s", opts)
       vim.keymap.set('n', '<leader>bv', "<C-w>v", opts)

       -- Disable arrow keys
       vim.keymap.set('n', '<Up>', "<Nop>", opts)
       vim.keymap.set('n', '<Down>', "<Nop>", opts)
       vim.keymap.set('n', '<Left>', "<Nop>", opts)
       vim.keymap.set('n', '<Right>', "<Nop>", opts)

       -- Show current buffer absolute path and copy it
       vim.keymap.set('n', '<F6>', function() vim.fn.setreg('+', vim.fn.expand('%:p')) print(vim.fn.getreg('+')) end, opts)

       -- Apply lsp server formating
       vim.keymap.set('n', '<leader>cf', function() vim.lsp.buf.format({ async = true }) end, opts)
    '';

  };
}
